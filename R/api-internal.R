# Internal Protocol Query Submission
# =============================================================================
# When a session is active, queries can be submitted via the internal
# /queries/v1/query-request endpoint instead of the public SQL API v2.
# This enables session-scoped operations (transactions, temp tables, session
# variables) and, in the future, native Arrow result transport.
#
# The internal endpoint uses the session token in the Authorization header
# and returns results in the internal format (which can include Arrow IPC
# data for Phase 5).

#' Submit a query via the internal protocol (session-based)
#'
#' @param conn SnowflakeConnection with an active session.
#' @param sql SQL string.
#' @param bindings Parameter bindings (same format as SQL API v2), or NULL.
#' @returns Parsed JSON response body.
#' @noRd
sf_internal_submit <- function(conn, sql, bindings = NULL) {
  .ensure_session_fresh(conn)

  session <- conn@.state$session
  host <- sf_host(conn@account)
  url <- paste0(host, "/queries/v1/query-request")

  body <- list(
    sqlText            = sql,
    sequenceId         = as.integer(Sys.time()),
    describeOnly       = FALSE,
    parameters         = list(
      QUERY_TIMEOUT    = getOption("skiLift.timeout", 600L)
    )
  )

  if (nzchar(conn@database))  body$database  <- conn@database
  if (nzchar(conn@schema))    body$schema    <- conn@schema
  if (nzchar(conn@warehouse)) body$warehouse <- conn@warehouse
  if (nzchar(conn@role))      body$role      <- conn@role

  if (!is.null(bindings)) {
    body$bindings <- bindings
  }

  req <- httr2::request(url) |>
    httr2::req_headers(
      "Authorization" = paste("Snowflake Token=", paste0('"', session$token, '"')),
      "Content-Type"  = "application/json",
      "Accept"        = "application/json",
      "User-Agent"    = sf_user_agent()
    ) |>
    httr2::req_body_json(body, auto_unbox = TRUE) |>
    httr2::req_timeout(getOption("skiLift.timeout", 600L)) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_retry(
      max_tries    = getOption("skiLift.retry_max", 3L),
      is_transient = function(resp) httr2::resp_status(resp) %in% c(429L, 503L)
    )

  resp <- tryCatch(
    httr2::req_perform(req),
    error = function(e) {
      cli_abort(c(
        "x" = "Internal query request failed.",
        "x" = conditionMessage(e)
      ))
    }
  )

  status <- httr2::resp_status(resp)
  resp_body <- .parse_json_body(resp)

  if (status == 200L && isTRUE(resp_body$success)) {
    return(resp_body)
  }

  # Handle 401 with session renewal
 if (status == 401L) {
    conn@.state$session <- sf_session_renew(conn@account, session)
    return(sf_internal_submit(conn, sql, bindings))
  }

  err_msg <- resp_body$message %||%
             resp_body$data$errorMessage %||%
             "(unknown error)"
  sf_code <- resp_body$data$errorCode %||% as.character(status)

  cli_abort(c(
    "x" = "Snowflake internal query error (HTTP {status}, code {sf_code}).",
    "i" = err_msg
  ))
}


#' Submit a statement (DML/DDL/transaction control) via internal protocol
#'
#' Thin wrapper that's used for BEGIN/COMMIT/ROLLBACK and other statements
#' where we don't need to parse result data.
#'
#' @param conn SnowflakeConnection with an active session.
#' @param sql SQL string.
#' @returns Parsed response body.
#' @noRd
sf_internal_execute <- function(conn, sql) {
  sf_internal_submit(conn, sql)
}


#' Submit a query requesting Arrow result format via the internal protocol
#'
#' Same as sf_internal_submit but includes the arrow result type hint
#' so the response includes `rowsetBase64` (Arrow IPC) instead of JSON data.
#'
#' @param conn SnowflakeConnection with an active session.
#' @param sql SQL string.
#' @param bindings Parameter bindings, or NULL.
#' @returns Parsed JSON response body with `rowsetBase64` and `chunks`.
#' @noRd
sf_internal_submit_arrow <- function(conn, sql, bindings = NULL) {
  .ensure_session_fresh(conn)

  session <- conn@.state$session
  host <- sf_host(conn@account)
  url <- paste0(host, "/queries/v1/query-request")

  body <- list(
    sqlText            = sql,
    sequenceId         = as.integer(Sys.time()),
    describeOnly       = FALSE,
    parameters         = list(
      QUERY_TIMEOUT           = getOption("skiLift.timeout", 600L),
      CLIENT_RESULT_CHUNK_SIZE = 0L
    )
  )

  if (nzchar(conn@database))  body$database  <- conn@database
  if (nzchar(conn@schema))    body$schema    <- conn@schema
  if (nzchar(conn@warehouse)) body$warehouse <- conn@warehouse
  if (nzchar(conn@role))      body$role      <- conn@role

  if (!is.null(bindings)) {
    body$bindings <- bindings
  }

  req <- httr2::request(url) |>
    httr2::req_headers(
      "Authorization" = paste("Snowflake Token=", paste0('"', session$token, '"')),
      "Content-Type"  = "application/json",
      "Accept"        = "application/snowflake",
      "User-Agent"    = sf_user_agent()
    ) |>
    httr2::req_body_json(body, auto_unbox = TRUE) |>
    httr2::req_timeout(getOption("skiLift.timeout", 600L)) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_retry(
      max_tries    = getOption("skiLift.retry_max", 3L),
      is_transient = function(resp) httr2::resp_status(resp) %in% c(429L, 503L)
    )

  resp <- tryCatch(
    httr2::req_perform(req),
    error = function(e) {
      cli_abort(c(
        "x" = "Internal Arrow query request failed.",
        "x" = conditionMessage(e)
      ))
    }
  )

  status <- httr2::resp_status(resp)
  resp_body <- .parse_json_body(resp)

  if (status == 200L && isTRUE(resp_body$success)) {
    return(resp_body)
  }

  if (status == 401L) {
    conn@.state$session <- sf_session_renew(conn@account, session)
    return(sf_internal_submit_arrow(conn, sql, bindings))
  }

  err_msg <- resp_body$message %||%
             resp_body$data$errorMessage %||%
             "(unknown error)"
  sf_code <- resp_body$data$errorCode %||% as.character(status)

  cli_abort(c(
    "x" = "Snowflake internal Arrow query error (HTTP {status}, code {sf_code}).",
    "i" = err_msg
  ))
}
