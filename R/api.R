# Low-Level Snowflake SQL API v2 Client
# =============================================================================

#' Submit a SQL statement to Snowflake
#'
#' @param con A SnowflakeConnection object.
#' @param sql SQL string.
#' @param bindings Named list of parameter bindings, or NULL.
#' @param async Logical. If TRUE, don't wait for completion.
#' @returns Parsed JSON response body (list).
#' @noRd
sf_api_submit <- function(con, sql, bindings = NULL, async = FALSE) {
  host <- sf_host(con@account, con@.auth)
  url <- paste0(host, "/api/v2/statements")

  body <- list(
    statement = sql,
    timeout   = getOption("RSnowflake.timeout", 600L),
    resultSetMetaData = list(format = "jsonv2")
  )

  if (nzchar(con@database))  body$database  <- con@database
  if (nzchar(con@schema))    body$schema    <- con@schema
  if (nzchar(con@warehouse)) body$warehouse <- con@warehouse
  if (nzchar(con@role))      body$role      <- con@role

  if (!is.null(bindings)) {
    body$bindings <- bindings
  }

  params <- list()
  if (async) params$async <- "true"
  if (length(params) > 0L) {
    qs <- paste0(names(params), "=", params, collapse = "&")
    url <- paste0(url, "?", qs)
  }

  .sf_api_request_with_refresh(con, "POST", url, body = body)
}

#' Fetch a result partition (JSON)
#' @noRd
sf_api_fetch_partition <- function(con, handle, partition) {
  host <- sf_host(con@account, con@.auth)
  url <- paste0(host, "/api/v2/statements/", handle,
                "?partition=", partition)
  .sf_api_request_with_refresh(con, "GET", url)
}

#' Check the status of an async statement
#' @noRd
sf_api_status <- function(con, handle) {
  host <- sf_host(con@account, con@.auth)
  url <- paste0(host, "/api/v2/statements/", handle)
  .sf_api_request_with_refresh(con, "GET", url)
}

#' Cancel a running statement
#' @noRd
sf_api_cancel <- function(con, handle) {
  host <- sf_host(con@account, con@.auth)
  url <- paste0(host, "/api/v2/statements/", handle, "/cancel")
  tryCatch(
    .sf_api_request_with_refresh(con, "POST", url),
    error = function(e) NULL
  )
}


# ---------------------------------------------------------------------------
# Token refresh wrapper
# ---------------------------------------------------------------------------

.sf_api_request_with_refresh <- function(con, method, url, body = NULL) {
  .refresh_token_if_stale(con)

  resp <- .sf_api_request_raw(con, method, url, body)
  status <- httr2::resp_status(resp)

  if (status == 401L) {
    refreshed <- .try_refresh_token(con)
    if (refreshed) {
      resp <- .sf_api_request_raw(con, method, url, body)
      status <- httr2::resp_status(resp)
    }
  }

  .handle_response(resp, url)
}

#' Re-read a file-sourced token if the source file has changed on disk
#'
#' Workspace / Native App platforms have been observed rotating the token
#' file well inside its own lifetime (~5 min against a 600s TTL). Stating the
#' file first is one syscall and avoids a wasted round trip through the
#' 401-retry on every rotation. Tokens sourced from an env var, or types with
#' no file (jwt, pat, explicit), have nothing to stat and fall through to the
#' 401-retry backstop instead.
#' @returns invisible(NULL). Updates con@.state on change.
#' @noRd
.refresh_token_if_stale <- function(con) {
  auth <- con@.auth
  if (!(auth$type %in% c("token", "oauth")) || is.null(auth$token_file)) {
    return(invisible(NULL))
  }

  current_mtime <- .file_mtime(auth$token_file)
  cached_mtime  <- con@.state$token_mtime
  if (!is.na(current_mtime) && !is.null(cached_mtime) &&
        !is.na(cached_mtime) && current_mtime <= cached_mtime) {
    return(invisible(NULL))
  }

  new_token <- .read_token_from_source(auth)
  if (!is.null(new_token) && nzchar(new_token)) {
    con@.state$token       <- new_token
    con@.state$token_mtime <- current_mtime
  }
  invisible(NULL)
}

#' Re-read a token from wherever `auth` says it came from
#'
#' `token_file` means different things for different sources: for a
#' Workspace bare-token file (`/snowflake/session/token`) the file's
#' entire content *is* the token, so a raw read is correct. For a
#' connections.toml OAuth profile, `token_file` is the path to the whole
#' TOML document -- reading it raw would hand the wire a token string
#' that is literally the file's account/authenticator/token lines
#' concatenated together, which is exactly the 390146 "Bearer token is
#' missing" failure this fixes. `token_source == "toml"` routes through
#' the real parser and re-selects the same profile by name instead.
#' @returns Character token, or NULL if it could not be re-read.
#' @noRd
.read_token_from_source <- function(auth) {
  if (identical(auth$token_source, "toml")) {
    profile <- sf_read_connections_toml(auth$token_profile)
    return(profile$token)
  }
  trimws(paste(readLines(auth$token_file, warn = FALSE), collapse = ""))
}

#' @returns Numeric mtime, or NA if `path` is NULL or missing.
#' @noRd
.file_mtime <- function(path) {
  if (is.null(path) || !file.exists(path)) return(NA_real_)
  as.numeric(file.mtime(path))
}

#' Attempt to refresh the auth token
#'
#' Writes into `con@.state`, not `con@.auth` -- `.auth` is a plain S4 list
#' slot (value-copied), so a write there is discarded on return and the
#' caller's retry would resend the stale token. `.state` is an environment
#' slot, so this mutation is visible to the caller.
#' @returns TRUE if token was refreshed, FALSE otherwise.
#' @noRd
.try_refresh_token <- function(con) {
  auth <- con@.auth

  if (auth$type == "jwt") {
    return(tryCatch({
      new_jwt <- sf_generate_jwt(auth$account, auth$user, auth$private_key_path)
      con@.state$token       <- new_jwt
      con@.state$token_mtime <- as.numeric(Sys.time())
      TRUE
    }, error = function(e) FALSE))
  }

  if (identical(auth$token_source, "toml")) {
    old_token <- con@.state$token %||% auth$token
    new_token <- .read_token_from_source(auth)
    token_changed <- !is.null(new_token) && nzchar(new_token) && new_token != old_token
    if (token_changed) {
      con@.state$token       <- new_token
      con@.state$token_mtime <- .file_mtime(auth$token_file)
    }
    return(token_changed)
  }

  if (auth$type %in% c("token", "oauth")) {
    old_token <- con@.state$token %||% auth$token
    ws <- .read_workspace_token()
    token_changed <- nzchar(ws$token) && ws$token != old_token
    if (token_changed) {
      con@.state$token       <- ws$token
      con@.state$token_mtime <- .file_mtime(ws$file)
    }
    return(token_changed)
  }

  FALSE
}


# ---------------------------------------------------------------------------
# Internal HTTP helper
# ---------------------------------------------------------------------------

.sf_api_request_raw <- function(con, method, url, body = NULL) {
  auth <- con@.auth
  # .state$token is the live copy -- see .try_refresh_token() -- and is only
  # unset for connections predating this cache (e.g. hand-built in tests).
  token <- con@.state$token %||% auth$token
  token_type <- auth$token_type %||% "KEYPAIR_JWT"

  req <- httr2::request(url) |>
    httr2::req_headers(
      "Authorization" = paste("Bearer", token),
      "X-Snowflake-Authorization-Token-Type" = token_type,
      "Content-Type"  = "application/json",
      "Accept"        = "application/json",
      "User-Agent"    = sf_user_agent()
    ) |>
    httr2::req_timeout(getOption("RSnowflake.timeout", 600L))

  if (!is.null(body)) {
    req <- req |> httr2::req_body_json(body, auto_unbox = TRUE)
  }

  if (toupper(method) == "GET") {
    req <- req |> httr2::req_method("GET")
  }

  max_retries <- getOption("RSnowflake.retry_max", 3L)
  req <- req |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_retry(
      max_tries    = max_retries,
      is_transient = function(resp) httr2::resp_status(resp) %in% c(429L, 503L)
    )

  tryCatch(
    httr2::req_perform(req),
    error = function(e) {
      cli_abort(c(
        "x" = "Snowflake API request failed.",
        "i" = "URL: {.url {url}}",
        "x" = conditionMessage(e)
      ))
    }
  )
}

.handle_response <- function(resp, url) {
  status <- httr2::resp_status(resp)

  if (status %in% c(200L, 202L)) {
    return(.parse_json_body(resp))
  }

  err_body <- tryCatch(.parse_json_body(resp), error = function(e) list())
  sf_code <- err_body$code %||% as.character(status)
  sf_msg  <- err_body$message %||%
             tryCatch(httr2::resp_body_string(resp), error = function(e) "(no body)")

  cli_abort(c(
    "x" = "Snowflake SQL API error (HTTP {status}, code {sf_code}).",
    "i" = sf_msg
  ))
}

#' Parse a JSON response body, using RcppSimdJson when available
#'
#' Uses fparse with default simplification for maximum speed.  Downstream
#' parsers (sf_parse_metadata, .json_data_to_df) handle both the simplified
#' structures (data.frame/matrix) from fparse and the nested-list structures
#' from httr2::resp_body_json().
#' @noRd
.parse_json_body <- function(resp) {
  use_simd <- isTRUE(getOption("RSnowflake.use_simdjson", TRUE))
  if (use_simd && requireNamespace("RcppSimdJson", quietly = TRUE)) {
    raw_bytes <- httr2::resp_body_raw(resp)
    return(RcppSimdJson::fparse(rawToChar(raw_bytes)))
  }
  httr2::resp_body_json(resp)
}
