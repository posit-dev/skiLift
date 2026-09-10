# Authentication Resolution
# =============================================================================

#' Resolve authentication for a Snowflake connection
#'
#' Determines the auth method and returns a list with `type`, `token`, and
#' `token_type` (the value for the X-Snowflake-Authorization-Token-Type header).
#'
#' Priority order:
#' 1. Explicit bearer token (the `token` parameter) -- always a PAT; a
#'    caller who wants OAuth semantics goes through the profile instead
#' 2. Workspace SPCS OAuth token (SNOWFLAKE_HOST set + token file exists) --
#'    all official Snowflake drivers accept this token when connecting via
#'    the internal SPCS gateway.  Returns type = "oauth" so that ADBC maps
#'    to auth_oauth and REST API v2 uses Bearer + OAUTH token type.
#' 3. External browser SSO (`authenticator = "externalbrowser"`), delegated
#'    to `snowflakeauth`. After the Workspace branch, because a container has
#'    no browser and its session token is the only valid credential there;
#'    before PAT, because an explicit `authenticator` should beat an ambient
#'    SNOWFLAKE_PAT in the environment.
#' 4. Programmatic Access Token (SNOWFLAKE_PAT env var)
#' 5. Key-pair JWT (private_key_path + account + user)
#' 6. Workspace session token fallback (SNOWFLAKE_TOKEN env var or token
#'    file without SNOWFLAKE_HOST) -- legacy path.
#' 7. connections.toml OAuth profile (e.g. Posit Workbench / the Native App,
#'    which write a short-lived OAuth token into the profile rather than
#'    exposing SNOWFLAKE_HOST or a session-token file) -- local, so it is
#'    last, after every ambient-environment check has had a chance to fire.
#'
#' @param account Account identifier.
#' @param user Username.
#' @param token Explicit bearer token.
#' @param private_key_path Path to PEM private key file.
#' @param authenticator Auth method string.
#' @param profile_token OAuth token read from a connections.toml profile,
#'   if the caller resolved one and no higher-priority credential exists.
#' @param profile_token_file Path to the connections.toml the profile came
#'   from, so a rotated token can be re-read from the same place.
#' @param profile_token_name The profile's name within that file (its
#'   `toml_name` attribute), so a refresh can re-select the same profile
#'   after re-parsing rather than guessing at the default again.
#' @param name Optional `connections.toml` profile name, forwarded to
#'   external browser SSO so `snowflakeauth` can resolve profile fields the
#'   caller did not supply explicitly.
#' @returns A list with `type` ("oauth", "jwt", "pat", "token",
#'   "externalbrowser"), and either `token` or -- for browser SSO -- a
#'   completed `headers` list,
#'   and -- for tokens that rotate -- `token_file` (the path to re-read)
#'   and `token_source`. `token_source` matters because `token_file`
#'   means two different things: for a Workspace bare-token file
#'   (`/snowflake/session/token`) the whole file *is* the token, so a raw
#'   read is right; for a connections.toml OAuth profile, `token_file` is
#'   the path to the whole TOML document, and refreshing means re-parsing
#'   it and re-selecting `token_profile`, not reading it raw -- see
#'   `.read_token_from_source()` in api.R. Workspace/SPCS OAuth
#'   additionally sets `host_eligible = TRUE`; this is what `sf_host()`
#'   requires before routing to `SNOWFLAKE_HOST` -- a toml-sourced OAuth
#'   token is `type == "oauth"` too, but is meant for the public
#'   endpoint, not the internal gateway, so it must not carry that flag.
#' @noRd
sf_auth_resolve <- function(account, user = NULL, token = NULL,
                            private_key_path = NULL,
                            authenticator = NULL,
                            profile_token = NULL,
                            profile_token_file = NULL,
                            profile_token_name = NULL,
                            name = NULL) {
  auth_lower <- tolower(authenticator %||% "")

  # Priority 1: Explicit bearer token
  if (!is.null(token) && nzchar(token)) {
    return(list(
      type = "token",
      token = token,
      token_type = "PROGRAMMATIC_ACCESS_TOKEN"
    ))
  }

  # Priority 2: Workspace SPCS OAuth -- preferred when inside SPCS container
  if (nzchar(Sys.getenv("SNOWFLAKE_HOST", ""))) {
    ws <- .read_workspace_token()
    if (nzchar(ws$token)) {
      return(list(
        type = "oauth",
        token = ws$token,
        token_type = "OAUTH",
        token_file = ws$file,
        token_source = "file",
        host_eligible = TRUE
      ))
    }
  }

  # Priority 3: External browser SSO
  #
  # Placed after the Workspace branch so a container never reaches it: browser
  # auth cannot work without a browser, and inside SPCS the session token is
  # the only valid credential. Placed before the PAT check because an explicit
  # `authenticator` should beat an ambient SNOWFLAKE_PAT in the environment.
  if (identical(auth_lower, "externalbrowser")) {
    return(sf_auth_externalbrowser(account = account, user = user, name = name))
  }

  # Priority 4: Programmatic Access Token (PAT)
  pat <- Sys.getenv("SNOWFLAKE_PAT", "")
  if (nzchar(pat)) {
    return(list(
      type = "pat",
      token = pat,
      token_type = "PROGRAMMATIC_ACCESS_TOKEN"
    ))
  }

  # Priority 5: Key-pair JWT
  if (!is.null(private_key_path) || auth_lower == "snowflake_jwt") {
    if (is.null(private_key_path) || !nzchar(private_key_path)) {
      cli_abort(c(
        "Key-pair auth requires {.arg private_key_path}.",
        "i" = "Set {.field private_key_path} in your connections.toml profile."
      ))
    }
    if (is.null(account) || !nzchar(account)) {
      cli_abort("Key-pair auth requires {.arg account}.")
    }
    if (is.null(user) || !nzchar(user)) {
      cli_abort("Key-pair auth requires {.arg user}.")
    }
    jwt <- sf_generate_jwt(account, user, private_key_path)
    return(list(
      type = "jwt",
      token = jwt,
      token_type = "KEYPAIR_JWT",
      account = account,
      user = user,
      private_key_path = private_key_path,
      generated_at = Sys.time()
    ))
  }

  # Priority 6: Workspace session token fallback (no SNOWFLAKE_HOST)
  ws <- .read_workspace_token()
  if (nzchar(ws$token)) {
    return(list(
      type = "token",
      token = ws$token,
      token_type = "OAUTH",
      token_file = ws$file,
      token_source = "file"
    ))
  }

  # Priority 7: connections.toml OAuth profile -- Posit Workbench / the
  # Native App write a short-lived OAuth token directly into the profile;
  # there is no SNOWFLAKE_HOST and no session-token file to find it by.
  # host_eligible is deliberately not set: this token is for the public
  # endpoint, never the internal SPCS gateway. token_source = "toml" is
  # what tells refresh to re-parse token_file rather than read it raw.
  if (!is.null(profile_token) && nzchar(profile_token) && auth_lower == "oauth") {
    return(list(
      type = "oauth",
      token = profile_token,
      token_type = "OAUTH",
      token_file = profile_token_file,
      token_source = "toml",
      token_profile = profile_token_name
    ))
  }

  cli_abort(c(
    "No Snowflake credentials found.",
    "i" = "For interactive SSO on a desktop, set",
    " " = "{.code authenticator = \"externalbrowser\"}.",
    "i" = "In Workspace Notebooks, ensure SNOWFLAKE_HOST is set and",
    " " = "/snowflake/session/token exists (automatic in SPCS).",
    "i" = "Otherwise provide {.arg token}, set {.envvar SNOWFLAKE_PAT},",
    " " = "or configure key-pair auth in {.file connections.toml}."
  ))
}


# ---------------------------------------------------------------------------
# External browser SSO
# ---------------------------------------------------------------------------

#' Resolve credentials via external browser SSO
#'
#' Delegates to `snowflakeauth`, which implements the whole flow: a localhost
#' redirect listener, Snowflake's proof-key exchange, and caching of both the
#' session token and the keyring-backed ID token so the browser is not
#' reopened on every connection.
#'
#' Returns a completed `headers` list rather than a raw token. The
#' Authorization scheme is not the same across auth methods -- browser SSO and
#' workload identity use `Snowflake Token="..."` where key-pair and OAuth use
#' `Bearer` -- and the header returned by `snowflakeauth` already encodes that
#' difference. Unwrapping it here would mean reimplementing the distinction.
#'
#' Not reachable inside SPCS: there is no browser in a container, and the
#' Workspace branch of `sf_auth_resolve()` takes priority there.
#'
#' No raw token is exposed, so the 401-retry backstop cannot re-derive one the
#' way it does for JWT. Instead the connection params are returned alongside
#' the headers, and `.try_refresh_token()` re-requests credentials through
#' `snowflakeauth` for the same identity -- which renews from its own cache
#' when it can, and only falls back to the browser when the cached ID token
#' has itself expired.
#'
#' @param account Account identifier.
#' @param user Username (optional; resolved from the profile when absent).
#' @param name Optional `connections.toml` profile name.
#' @returns A list with `type`, `headers` and identifying fields.
#' @noRd
sf_auth_externalbrowser <- function(account, user = NULL, name = NULL) {
  if (!requireNamespace("snowflakeauth", quietly = TRUE)) {
    cli_abort(c(
      "External browser authentication requires the {.pkg snowflakeauth} package.",
      "i" = 'Install it with {.code install.packages("snowflakeauth")}.'
    ))
  }
  if (!requireNamespace("httpuv", quietly = TRUE)) {
    cli_abort(c(
      "External browser authentication requires the {.pkg httpuv} package.",
      "i" = 'Install it with {.code install.packages("httpuv")}.',
      " " = "It runs the localhost listener that receives the SSO redirect."
    ))
  }
  if (is.null(account) || !nzchar(account)) {
    cli_abort("External browser authentication requires {.arg account}.")
  }
  if (!rlang::is_interactive()) {
    cli_abort(c(
      "External browser authentication requires an interactive R session.",
      "i" = "For scripts and pipelines use key-pair JWT or a PAT."
    ))
  }

  # Build params through snowflakeauth's own constructor rather than by hand:
  # its session cache keys on hash(params), so a differently shaped list would
  # miss the cache and reopen the browser on every connection.
  params <- snowflakeauth::snowflake_connection(
    name = name,
    account = account,
    user = user,
    authenticator = "externalbrowser"
  )
  headers <- snowflakeauth::snowflake_credentials(params)

  list(
    type = "externalbrowser",
    headers = headers,
    # Kept so .try_refresh_token() can re-request credentials for the same
    # identity without rebuilding the params (and so without missing
    # snowflakeauth's hash(params)-keyed cache).
    params = params,
    account = account,
    user = user,
    generated_at = Sys.time()
  )
}


# ---------------------------------------------------------------------------
# Workspace Notebook helpers
# ---------------------------------------------------------------------------

#' Read the session token from env var or /snowflake/session/token file
#'
#' Also reports which file (if any) the token came from, so refresh can
#' re-read the same source and stat it for rotation ahead of a 401.
#' @returns list(token = <chr, possibly empty>, file = <path or NULL>).
#' @noRd
.read_workspace_token <- function() {
  tok <- Sys.getenv("SNOWFLAKE_TOKEN", "")
  if (nzchar(tok)) return(list(token = tok, file = NULL))

  token_file <- "/snowflake/session/token"
  if (file.exists(token_file)) {
    tok <- trimws(paste(readLines(token_file, warn = FALSE), collapse = ""))
    if (nzchar(tok)) return(list(token = tok, file = token_file))
  }
  list(token = "", file = NULL)
}

#' Detect whether we are running inside a Snowflake Workspace Notebook
#' @noRd
.is_workspace <- function() {
  nzchar(Sys.getenv("SNOWFLAKE_HOST", "")) ||
    file.exists("/snowflake/session/token")
}

#' Resolve the Snowflake account identifier in a Workspace Notebook
#' @noRd
.resolve_workspace_account <- function() {
  acct <- Sys.getenv("SNOWFLAKE_ACCOUNT", "")
  if (nzchar(acct)) return(acct)

  host <- Sys.getenv("SNOWFLAKE_HOST", "")
  if (nzchar(host)) {
    return(sub("\\.snowflakecomputing\\.com$", "", host))
  }

  if (requireNamespace("reticulate", quietly = TRUE)) {
    acct <- tryCatch({
      ctx <- reticulate::import("snowflake.snowpark.context")
      session <- ctx$get_active_session()
      gsub('"', '', session$get_current_account())
    }, error = function(e) NULL)
    if (!is.null(acct) && nzchar(acct)) return(acct)
  }

  cli_abort(c(
    "Cannot determine Snowflake account in Workspace Notebook.",
    "i" = "Set {.envvar SNOWFLAKE_ACCOUNT} or pass {.arg account} explicitly."
  ))
}


# ---------------------------------------------------------------------------
# connections.toml reader
# ---------------------------------------------------------------------------

#' Read a connection profile from connections.toml
#'
#' @param name Profile name, or NULL for default.
#' @returns Named list of connection parameters, or NULL. The list carries
#'   `toml_file` and `toml_name` attributes recording where it came from and
#'   which profile was selected -- rotation (`.refresh_token_if_stale()`)
#'   needs the path, and diagnostics benefit from the name being explicit
#'   rather than re-derived.
#' @noRd
sf_read_connections_toml <- function(name = NULL) {
  toml_dir <- Sys.getenv("SNOWFLAKE_HOME",
                          file.path(Sys.getenv("HOME"), ".snowflake"))
  toml_file <- file.path(toml_dir, "connections.toml")
  if (!file.exists(toml_file)) return(NULL)

  toml <- tryCatch(
    {
      if (requireNamespace("RcppTOML", quietly = TRUE)) {
        RcppTOML::parseTOML(toml_file)
      } else {
        .parse_toml_simple(toml_file)
      }
    },
    error = function(e) NULL
  )
  if (is.null(toml) || length(toml) == 0L) return(NULL)

  selected_name <- NULL
  profile <- NULL

  if (!is.null(name) && name %in% names(toml)) {
    selected_name <- name
    profile <- toml[[name]]
  } else if ("default" %in% names(toml)) {
    selected_name <- "default"
    profile <- toml[["default"]]
  } else if (length(toml) == 1L) {
    selected_name <- names(toml)[[1L]]
    profile <- toml[[1L]]
  } else {
    selected_name <- names(toml)[[1L]]
    cli_inform(c(
      "i" = "Using first profile {.val {selected_name}} from {.file connections.toml}.",
      "i" = "Pass {.arg name} to select a specific profile."
    ))
    profile <- toml[[1L]]
  }

  attr(profile, "toml_file") <- toml_file
  attr(profile, "toml_name") <- selected_name
  profile
}

#' Minimal TOML parser for simple key=value sections
#'
#' Handles the subset of TOML we need: `[section]` headers and
#' `key = "value"` pairs. No nested tables, no arrays.
#' @noRd
.parse_toml_simple <- function(path) {
  lines <- readLines(path, warn = FALSE)
  result <- list()
  current_section <- NULL

  for (line in lines) {
    line <- trimws(line)
    if (!nzchar(line) || startsWith(line, "#")) next

    # Section header
    m <- regmatches(line, regexpr("^\\[([^]]+)\\]$", line))
    if (length(m) == 1L && nzchar(m)) {
      current_section <- gsub("^\\[|\\]$", "", m)
      result[[current_section]] <- list()
      next
    }

    # key = value
    if (!is.null(current_section) && grepl("=", line, fixed = TRUE)) {
      parts <- strsplit(line, "=", fixed = TRUE)[[1L]]
      key <- trimws(parts[1L])
      val <- trimws(paste(parts[-1L], collapse = "="))
      val <- gsub('^"|"$', "", val)
      val <- gsub("^'|'$", "", val)
      result[[current_section]][[key]] <- val
    }
  }
  result
}
