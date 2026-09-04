# SnowflakeDriver S4 Class
# =============================================================================

#' SnowflakeDriver
#'
#' An S4 class representing the Snowflake DBI driver.
#'
#' @param dbObj A [SnowflakeDriver-class] object.
#' @param object A [SnowflakeDriver-class] object (for `show`).
#' @export
setClass("SnowflakeDriver", contains = "DBIDriver")

#' Create a SnowflakeDriver instance
#'
#' @returns A SnowflakeDriver singleton.
#' @export
#' @examples
#' \dontrun{
#' drv <- Snowflake()
#' con <- dbConnect(drv, account = "myaccount", name = "default")
#' }
Snowflake <- function() {
  new("SnowflakeDriver")
}

#' @rdname SnowflakeDriver-class
#' @export
setMethod("dbGetInfo", "SnowflakeDriver", function(dbObj, ...) {
  list(
    driver.version = utils::packageVersion("skiLift"),
    client.version = utils::packageVersion("skiLift"),
    max.connections = Inf
  )
})

#' @rdname SnowflakeDriver-class
#' @export
setMethod("dbIsValid", "SnowflakeDriver", function(dbObj, ...) {
  TRUE
})

#' @rdname SnowflakeDriver-class
#' @export
setMethod("dbUnloadDriver", "SnowflakeDriver", function(drv, ...) {
  invisible(TRUE)
})

#' @rdname SnowflakeDriver-class
#' @export
setMethod("show", "SnowflakeDriver", function(object) {
  cat("<SnowflakeDriver>\n")
})

#' @rdname SnowflakeDriver-class
#' @param obj An R object to map to a Snowflake SQL type.
#' @export
setMethod("dbDataType", "SnowflakeDriver", function(dbObj, obj, ...) {
  r_to_sf_type(obj)
})

#' Merge dbConnect() arguments with Workspace auto-detection and
#' connections.toml
#'
#' Kept separate from dbConnect() so that this resolution -- including
#' reading `profile$token` from connections.toml, the P1-02 fix -- is
#' testable without a live account. dbConnect() itself still needs one, to
#' run its validation query.
#' @returns list(account, user, database, schema, warehouse, role,
#'   authenticator, private_key_path, profile_token, profile_token_file).
#' @noRd
.resolve_connect_params <- function(account, user, token, private_key_path,
                                     authenticator, database, schema,
                                     warehouse, role, name) {
  # Workspace Notebook auto-detection (env var, token file, or host)
  if (is.null(token) && is.null(account) && .is_workspace()) {
    account   <- .resolve_workspace_account()
    database  <- if (nzchar(database))  database  else Sys.getenv("SNOWFLAKE_DATABASE", "")
    schema    <- if (nzchar(schema))    schema    else Sys.getenv("SNOWFLAKE_SCHEMA", "")
    warehouse <- if (nzchar(warehouse)) warehouse else Sys.getenv("SNOWFLAKE_WAREHOUSE", "")
    role      <- if (nzchar(role))      role      else Sys.getenv("SNOWFLAKE_ROLE", "")
    user      <- user %||% Sys.getenv("SNOWFLAKE_USER", "")
  }

  # Resolve parameters from connections.toml if not given explicitly.
  #
  # Read even when account is already known, as long as a profile name
  # was given explicitly: dbConnect(account = ..., name = "workbench")
  # must still pick up that profile's token, or profile$token stays
  # unreachable whenever a caller pins the account and relies on `name`
  # for credentials -- exactly the Posit Workbench / Native App shape.
  # Not widened to "whenever token is missing": that would merge in a
  # same-named default profile's credentials for an account the caller
  # never asked it for.
  profile_token <- NULL
  profile_token_file <- NULL
  profile_token_name <- NULL
  if (is.null(account) || !is.null(name)) {
    profile <- sf_read_connections_toml(name)
    if (!is.null(profile)) {
      account          <- account %||% profile$account
      user             <- user %||% profile$user
      authenticator    <- authenticator %||% profile$authenticator
      private_key_path <- private_key_path %||% profile$private_key_path
      database         <- if (nzchar(database)) database else (profile$database %||% "")
      schema           <- if (nzchar(schema)) schema else (profile$schema %||% "")
      warehouse        <- if (nzchar(warehouse)) warehouse else (profile$warehouse %||% "")
      role             <- if (nzchar(role)) role else (profile$role %||% "")
      if (is.null(token) && !is.null(profile$token) && nzchar(profile$token)) {
        profile_token      <- profile$token
        profile_token_file <- attr(profile, "toml_file")
        profile_token_name <- attr(profile, "toml_name")
      }
    }
  }

  list(
    account = account, user = user, database = database, schema = schema,
    warehouse = warehouse, role = role, authenticator = authenticator,
    private_key_path = private_key_path,
    profile_token = profile_token, profile_token_file = profile_token_file,
    profile_token_name = profile_token_name
  )
}

#' @rdname SnowflakeDriver-class
#' @param drv A SnowflakeDriver, or missing (uses default).
#' @param account Snowflake account identifier (e.g. "myaccount").
#' @param user Snowflake username.
#' @param token Explicit bearer token (PAT or session token).
#' @param private_key_path Path to PEM private key for JWT auth.
#' @param authenticator Auth method ("SNOWFLAKE_JWT", "OAUTH", etc.).
#' @param database Default database.
#' @param schema Default schema.
#' @param warehouse Default warehouse.
#' @param role Default role.
#' @param name Profile name from connections.toml.
#' @param ... Additional arguments (ignored).
#' @export
setMethod("dbConnect", "SnowflakeDriver",
  function(drv, account = NULL, user = NULL, token = NULL,
           private_key_path = NULL, authenticator = NULL,
           database = "", schema = "", warehouse = "", role = "",
           name = NULL, ...) {

    resolved <- .resolve_connect_params(
      account = account, user = user, token = token,
      private_key_path = private_key_path, authenticator = authenticator,
      database = database, schema = schema, warehouse = warehouse,
      role = role, name = name
    )
    account          <- resolved$account
    user             <- resolved$user
    database         <- resolved$database
    schema           <- resolved$schema
    warehouse        <- resolved$warehouse
    role             <- resolved$role
    authenticator    <- resolved$authenticator
    private_key_path <- resolved$private_key_path

    if (is.null(account) || !nzchar(account)) {
      cli_abort(c(
        "x" = "Snowflake {.arg account} is required.",
        "i" = "Pass {.arg account} directly or configure {.file connections.toml}."
      ))
    }

    auth <- sf_auth_resolve(
      account = account,
      user = user,
      token = token,
      private_key_path = private_key_path,
      authenticator = authenticator,
      profile_token = resolved$profile_token,
      profile_token_file = resolved$profile_token_file,
      profile_token_name = resolved$profile_token_name
    )

    con <- new("SnowflakeConnection",
      account   = account,
      user      = user %||% "",
      database  = database %||% "",
      schema    = schema %||% "",
      warehouse = warehouse %||% "",
      role      = role %||% "",
      .auth     = auth,
      .state    = .new_conn_state()
    )

    # Seed the live token cache -- see .new_conn_state() and .try_refresh_token().
    con@.state$token <- auth$token
    if (!is.null(auth$token_file)) {
      con@.state$token_mtime <- .file_mtime(auth$token_file)
    }

    # Optionally establish a persistent session for transactions & internal protocol
    use_session <- isTRUE(getOption("skiLift.use_session", FALSE))
    if (use_session) {
      tryCatch({
        session_info <- sf_session_login(account, auth)
        con@.state$session <- session_info
      }, error = function(e) {
        cli_warn(c(
          "!" = "Session login failed, falling back to stateless SQL API v2.",
          "i" = conditionMessage(e)
        ))
      })
    }

    # Validate the connection.
    #
    # In Workspace (SPCS OAuth):
    # - ADBC installed: all SQL via Go driver on SNOWFLAKE_HOST (Arrow-native).
    # - No ADBC: REST API v2 on SNOWFLAKE_HOST with Bearer + SPCS token.
    #   sf_host() routes OAuth to the internal gateway automatically.
    #
    # Outside Workspace: REST API v2 on the public endpoint as before.
    if (auth$type == "oauth" && .adbc_packages_available()) {
      tryCatch({
        adbc <- .init_adbc_backend(con)
        if (!is.null(adbc)) {
          con@.state$adbc <- adbc
          cli_inform(c(
            "v" = "Connected to Snowflake account {.val {account}} (ADBC, SPCS OAuth).",
            "i" = "Database: {.val {database}}, Warehouse: {.val {warehouse}}"
          ))
        } else {
          cli_abort("ADBC backend init returned NULL.")
        }
      }, error = function(e) {
        cli_abort(c(
          "x" = "Failed to connect to Snowflake account {.val {account}} via ADBC.",
          "x" = conditionMessage(e),
          "i" = "Verify SNOWFLAKE_HOST is set and /snowflake/session/token exists."
        ))
      })
    } else {
      tryCatch({
        resp <- sf_api_submit(con, "SELECT CURRENT_VERSION() AS version")
        con@.state$session_info <- resp
        mode <- if (auth$type == "oauth") " (REST, SPCS OAuth)" else ""
        session_note <- if (.has_session(con)) " (session-based)" else ""
        cli_inform(c(
          "v" = "Connected to Snowflake account {.val {account}}{mode}{session_note}.",
          "i" = "Database: {.val {database}}, Warehouse: {.val {warehouse}}"
        ))
      }, error = function(e) {
        cli_abort(c(
          "x" = "Failed to connect to Snowflake account {.val {account}}.",
          "x" = conditionMessage(e)
        ))
      })
    }

    .on_connection_opened(con)
    con
  }
)

#' @rdname SnowflakeDriver-class
#' @export
setMethod("dbCanConnect", "SnowflakeDriver",
  function(drv, ...) {
    tryCatch({
      con <- dbConnect(drv, ...)
      dbDisconnect(con)
      TRUE
    }, error = function(e) FALSE)
  }
)

#' Connect to Snowflake using a profile name
#'
#' Convenience wrapper around `dbConnect(Snowflake(), name = ...)`.
#'
#' @param name Profile name from connections.toml.
#' @param ... Additional arguments passed to dbConnect.
#' @returns A SnowflakeConnection.
#' @export
sf_connect <- function(name = NULL, ...) {
  dbConnect(Snowflake(), name = name, ...)
}
