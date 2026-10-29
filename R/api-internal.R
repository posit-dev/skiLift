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
  .sf_internal_request(conn, sql, bindings, arrow = FALSE)
}

#' One request to the internal query endpoint
#'
#' Snowflake rejects a query request that has no `requestId` (HTTP 400, empty
#' body), so every request carries one. The session is renewed once on a 401;
#' a second 401 is an error, not another renewal -- an unbounded retry here
#' used to recurse until R ran out of stack.
#' @param arrow TRUE to ask for Arrow IPC (`rowsetBase64`) instead of JSON rows.
#' @noRd
.sf_internal_request <- function(conn, sql, bindings = NULL, arrow = FALSE) {
  .ensure_session_fresh(conn)

  host <- sf_host(conn@account)
  url <- paste0(host, "/queries/v1/query-request")

  parameters <- list(QUERY_TIMEOUT = getOption("skiLift.timeout", 600L))
  if (arrow) parameters$CLIENT_RESULT_CHUNK_SIZE <- 0L

  body <- list(
    sqlText      = sql,
    sequenceId   = as.integer(Sys.time()),
    describeOnly = FALSE,
    parameters   = parameters
  )
  if (nzchar(conn@database))  body$database  <- conn@database
  if (nzchar(conn@schema))    body$schema    <- conn@schema
  if (nzchar(conn@warehouse)) body$warehouse <- conn@warehouse
  if (nzchar(conn@role))      body$role      <- conn@role
  if (!is.null(bindings)) body$bindings <- bindings

  # Read the session token when the request is built, so a renewal between
  # attempts is picked up.
  perform <- function(u, timeout) {
    token <- conn@.state$session$token
    httr2::request(u) |>
      httr2::req_headers(
        "Authorization" = .session_auth_header(token),
        "Content-Type"  = "application/json",
        "Accept"        = if (arrow) "application/snowflake" else "application/json",
        "User-Agent"    = sf_user_agent()
      ) |>
      httr2::req_body_json(body, auto_unbox = TRUE) |>
      httr2::req_timeout(timeout) |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform()
  }

  request_id <- .new_request_id()
  deadline <- .sf_deadline()
  send <- function() {
    .sf_send(conn, "POST", url, request_id = request_id, deadline = deadline,
             perform = perform, safe_retry = FALSE)
  }

  resp <- send()
  if (httr2::resp_status(resp) == 401L) {
    conn@.state$session <- sf_session_renew(conn@account, conn@.state$session)
    resp <- send()
  }

  status <- httr2::resp_status(resp)
  resp_body <- tryCatch(.parse_json_body(resp), error = function(e) list())

  if (status == 200L && isTRUE(resp_body$success)) {
    return(resp_body)
  }

  err_msg <- resp_body$message %||%
             resp_body$data$errorMessage %||%
             "(unknown error)"
  sf_code <- resp_body$data$errorCode %||% as.character(status)
  what <- if (arrow) "internal Arrow query" else "internal query"
  meta <- attr(resp, "skilift")

  .sf_abort(
    c(
      "x" = "Snowflake {what} error (HTTP {status}, code {sf_code}).",
      "i" = "{err_msg}",
      .sf_hints(status, "session", message = err_msg)
    ),
    # This endpoint reports a failed statement as HTTP 200 with success = false.
    class = if (status == 200L) c("skilift_error_sql", "skilift_error_http") else .sf_http_class(status),
    status = status, code = sf_code,
    sql_state = resp_body$data$sqlState,
    request_id = meta$request_id, attempts = meta$attempts,
    call = NULL
  )
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
  .sf_internal_request(conn, sql, bindings, arrow = TRUE)
}
