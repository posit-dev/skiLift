# Low-Level Snowflake SQL API v2 Client
# =============================================================================

#' Submit a SQL statement to Snowflake and wait for it to finish
#'
#' Snowflake answers a statement that runs longer than 45 seconds with HTTP 202
#' and a handle instead of a result. Returning that to the caller would look
#' like an empty result, so this polls until the statement has finished (see
#' `.sf_poll()`), cancels it if the deadline passes or the user interrupts, and
#' only then returns.
#'
#' Every statement carries a `requestId`. If the request has to be sent again
#' (a 503, a dropped connection), the same id goes with `retry=true`, so
#' Snowflake does not run the statement a second time -- see `.sf_send()`.
#'
#' @param con A SnowflakeConnection object.
#' @param sql SQL string.
#' @param bindings Named list of parameter bindings, or NULL.
#' @param async Logical. If TRUE, return as soon as Snowflake has accepted the
#'   statement (the body then holds the statement handle, not a result).
#' @returns Parsed JSON response body (list).
#' @noRd
sf_api_submit <- function(con, sql, bindings = NULL, async = FALSE) {
  host <- sf_host(con@account, con@.auth)
  url <- paste0(host, "/api/v2/statements")

  body <- list(
    statement = sql,
    timeout   = getOption("skiLift.timeout", 600L),
    resultSetMetaData = list(format = "jsonv2"),
    # BINARY comes back as hex text; the account or user could set BASE64
    parameters = list(binary_output_format = "HEX")
  )

  if (nzchar(con@database))  body$database  <- con@database
  if (nzchar(con@schema))    body$schema    <- con@schema
  if (nzchar(con@warehouse)) body$warehouse <- con@warehouse
  if (nzchar(con@role))      body$role      <- con@role

  if (!is.null(bindings)) {
    body$bindings <- bindings
  }

  if (async) url <- paste0(url, "?async=true")

  request_id <- .new_request_id()
  deadline <- .sf_deadline()
  resp_body <- .sf_api_request_with_refresh(
    con, "POST", url, body = body, request_id = request_id, deadline = deadline,
    request_timeout = .sf_request_timeout()
  )

  if (async || !identical(attr(resp_body, "http_status"), 202L)) {
    return(resp_body)
  }
  .sf_await(con, resp_body, deadline, request_id)
}

#' Fetch a result partition (JSON)
#'
#' A partition can also come back as 202 (still being prepared) or 429 (rate
#' limited) under load; both mean "ask again", so they are polled the same way
#' as a running statement. The statement itself has finished, so it is never
#' cancelled from here.
#' @noRd
sf_api_fetch_partition <- function(con, handle, partition) {
  host <- sf_host(con@account, con@.auth)
  url <- paste0(host, "/api/v2/statements/", handle,
                "?partition=", partition)
  deadline <- .sf_deadline()
  resp <- .sf_api_request_with_refresh(con, "GET", url, deadline = deadline, raw = TRUE)
  status <- httr2::resp_status(resp)
  if (status %in% c(202L, 429L)) {
    return(.sf_poll(con, url, deadline, handle = handle, cancel = FALSE))
  }
  .handle_response(resp, url, con)
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
    .sf_api_request_with_refresh(con, "POST", url, request_timeout = 30),
    error = function(e) NULL
  )
}


# ---------------------------------------------------------------------------
# Waiting for a statement that has not finished
# ---------------------------------------------------------------------------

#' Wait for a statement Snowflake answered with 202
#' @noRd
.sf_await <- function(con, resp_body, deadline, request_id = NULL) {
  handle <- resp_body$statementHandle
  host <- sf_host(con@account, con@.auth)
  url <- paste0(host, "/api/v2/statements/", handle)
  .sf_poll(con, url, deadline, handle = handle, request_id = request_id,
           cancel = TRUE)
}

#' Ask again until the answer is a result or an error
#'
#' 202 and 429 mean "not yet". Waits grow from 0.5 to 5 seconds. With
#' `cancel = TRUE`, anything that leaves this function before the statement has
#' finished -- the deadline, a network failure, or the user pressing Esc --
#' cancels it, so it does not keep running (and billing) in Snowflake.
#' A statement that failed with a SQL error (422) has finished and is not
#' cancelled.
#' @returns The parsed 200 body.
#' @noRd
.sf_poll <- function(con, url, deadline, handle = NULL, request_id = NULL,
                     cancel = TRUE) {
  finished <- FALSE
  if (cancel && !is.null(handle)) {
    on.exit(if (!finished) sf_api_cancel(con, handle), add = TRUE)
  }

  timed_out <- function() {
    timeout <- getOption("skiLift.timeout", 600L)
    .sf_abort(
      c(
        "x" = "Snowflake statement did not finish within {timeout} seconds{if (cancel) ' and was cancelled' else ''}.",
        "i" = "Statement handle: {.val {handle}}",
        if (!is.null(request_id)) c("i" = "Request id: {.val {request_id}}"),
        .sf_hints(kind = "timeout")
      ),
      class = "skilift_error_timeout",
      statement_handle = handle, request_id = request_id, timeout = timeout,
      call = NULL
    )
  }

  wait <- 0.5
  repeat {
    if (.sf_seconds_left(deadline) <= 0) timed_out()

    .sf_sleep(min(wait, max(0, .sf_seconds_left(deadline))))
    wait <- min(wait * 1.5, 5)
    # The sleep can run to the deadline; say so here, with the handle, rather
    # than letting the send fail first with a less useful message.
    if (.sf_seconds_left(deadline) <= 0) timed_out()

    resp <- .sf_api_request_with_refresh(
      con, "GET", url, deadline = deadline, raw = TRUE,
      request_timeout = .sf_request_timeout()
    )
    status <- httr2::resp_status(resp)
    if (status %in% c(202L, 429L)) next

    finished <- status %in% c(200L, 422L)
    return(.handle_response(resp, url, con))
  }
}


# ---------------------------------------------------------------------------
# Request ids, retries and deadlines
# ---------------------------------------------------------------------------

#' A random (version 4) UUID, from openssl so the user's random seed is untouched
#' @noRd
.new_request_id <- function() {
  b <- openssl::rand_bytes(16L)
  b[7L] <- as.raw(bitwOr(bitwAnd(as.integer(b[7L]), 0x0fL), 0x40L))
  b[9L] <- as.raw(bitwOr(bitwAnd(as.integer(b[9L]), 0x3fL), 0x80L))
  h <- paste(as.character(b), collapse = "")
  paste(substr(h, 1, 8), substr(h, 9, 12), substr(h, 13, 16),
        substr(h, 17, 20), substr(h, 21, 32), sep = "-")
}

# A function, not a bare Sys.sleep, so tests can replace it and run instantly.
.sf_sleep <- function(seconds) Sys.sleep(seconds)

# A number in [0, 1] that does not touch R's random seed.
.rand_unit <- function() as.integer(openssl::rand_bytes(1L)) / 255

#' Seconds to wait before attempt n + 1: jittered exponential, at least Retry-After
#' @noRd
.sf_backoff <- function(attempt, retry_after = NULL) {
  wait <- min(30, 0.5 * 2^(attempt - 1L)) * (0.5 + 0.5 * .rand_unit())
  if (length(retry_after) == 1L && !is.na(retry_after)) wait <- max(wait, retry_after)
  wait
}

# Seconds from a Retry-After header, or NA when there is none (or it is a date).
.sf_retry_after <- function(resp) {
  value <- httr2::resp_header(resp, "Retry-After")
  if (is.null(value)) return(NA_real_)
  suppressWarnings(as.numeric(value))
}

# Seconds one request may wait for an answer before it is abandoned and sent
# again (see .sf_send()). Not used for downloading a result partition, which can
# legitimately take longer.
.sf_request_timeout <- function() getOption("skiLift.request_timeout", 90L)

# One deadline per statement: skiLift.timeout seconds from submission, however
# many attempts and polls that takes.
.sf_deadline <- function() Sys.time() + getOption("skiLift.timeout", 600L)

.sf_seconds_left <- function(deadline) {
  if (is.null(deadline)) Inf else as.numeric(deadline - Sys.time(), units = "secs")
}

.with_request_id <- function(url, request_id, retry = FALSE) {
  if (is.null(request_id)) return(url)
  sep <- if (grepl("?", url, fixed = TRUE)) "&" else "?"
  paste0(url, sep, "requestId=", request_id, if (retry) "&retry=true" else "")
}

.tag_response <- function(resp, request_id, attempts) {
  attr(resp, "skilift") <- list(request_id = request_id, attempts = attempts)
  resp
}

#' Send a request, retrying what is safe to retry
#'
#' Retried: 429, 500, 502, 503, 504, and failures with no response (connection
#' reset, DNS, TLS, timeout). Not retried: 400, 401 (the caller refreshes the
#' credentials), 403, 404, 422.
#'
#' A statement POST has a `request_id`. The first attempt sends
#' `?requestId=<id>`; every later attempt sends `?requestId=<id>&retry=true`, so
#' Snowflake runs the statement once even if the first attempt did reach it
#' ("Snowflake does not execute the statement again if the statement has
#' already been executed successfully"). Without that, a 503 after the INSERT
#' had run would write the rows twice. GETs have no id: repeating them is
#' harmless.
#'
#' Tries and time are both bounded: `skiLift.retry_max` attempts, and the
#' statement's `deadline` (a wait that would pass it ends the retrying).
#'
#' `request_timeout` caps how long ONE request may wait for an answer. Snowflake
#' answers a statement POST within about 45 seconds (with the result, or a 202),
#' and a status check within seconds, so a request silent for much longer has
#' hung. Waiting out the whole `skiLift.timeout` (600 s) for it was the
#' observed failure -- a request with "0 bytes received" for ten minutes or more.
#' It is abandoned and sent again, which is safe: it carries the same request id.
#'
#' @param perform `function(url, timeout)` returning an httr2 response. The
#'   default sends a SQL API request for `con`.
#' @param safe_retry TRUE for the SQL API, where `retry=true` makes a resend
#'   safe. FALSE for the internal session protocol, which has no such
#'   documented guarantee: there only 429 and 503 are retried (as before), a
#'   failure with no response is not, and no `retry=true` is sent.
#' @returns An httr2 response, tagged with the request id and attempt count. A
#'   transient status that outlasted the retries is returned for the caller to
#'   turn into an error; a failure with no response is raised here.
#' @noRd
.sf_send <- function(con, method, url, body = NULL, request_id = NULL,
                     deadline = NULL, perform = NULL, safe_retry = TRUE,
                     request_timeout = NULL) {
  transient <- if (safe_retry) c(429L, 500L, 502L, 503L, 504L) else c(429L, 503L)
  max_tries <- max(1L, as.integer(getOption("skiLift.retry_max", 3L)))
  if (is.null(perform)) {
    perform <- function(u, timeout) .sf_api_request_raw(con, method, u, body, timeout = timeout)
  }

  attempt <- 0L
  repeat {
    attempt <- attempt + 1L
    left <- .sf_seconds_left(deadline)
    if (left <= 0) {
      .sf_abort(
        c(
          "x" = "Snowflake request ran out of time before it could be sent.",
          if (!is.null(request_id)) c("i" = "Request id: {.val {request_id}}"),
          .sf_hints(kind = "timeout")
        ),
        class = "skilift_error_timeout",
        request_id = request_id, attempts = attempt - 1L, call = NULL
      )
    }
    timeout <- min(getOption("skiLift.timeout", 600L), request_timeout %||% Inf,
                   max(1, left))

    out <- tryCatch(
      perform(.with_request_id(url, request_id, retry = safe_retry && attempt > 1L), timeout),
      error = function(e) e
    )
    failed <- inherits(out, "error")
    status <- if (failed) NA_integer_ else httr2::resp_status(out)

    if (failed && !safe_retry) .sf_abort_failed(out, url, request_id, attempt, con)
    if (!(failed || status %in% transient)) {
      return(.tag_response(out, request_id, attempt))
    }

    wait <- .sf_backoff(attempt, if (!failed) .sf_retry_after(out))
    if (attempt >= max_tries || wait >= .sf_seconds_left(deadline)) {
      if (failed) .sf_abort_failed(out, url, request_id, attempt, con)
      return(.tag_response(out, request_id, attempt))
    }
    .sf_sleep(wait)
  }
}

#' Raise the error for a request that never got a response
#' @noRd
.sf_abort_failed <- function(e, url, request_id, attempts, con = NULL) {
  timed_out <- grepl("timeout was reached|timed out", conditionMessage(e),
                     ignore.case = TRUE)
  .sf_abort(
    c(
      "x" = "Snowflake API request failed.",
      "i" = "URL: {.url {url}}",
      "i" = "Attempts: {attempts}; request id: {.val {request_id}}",
      if (timed_out) .sf_hints(kind = "timeout") else .sf_hints(NA)
    ),
    class = c(if (timed_out) "skilift_error_timeout", "skilift_error_retry"),
    url = url, request_id = request_id, attempts = attempts,
    parent = e, call = NULL
  )
}


# ---------------------------------------------------------------------------
# Token refresh wrapper
# ---------------------------------------------------------------------------

#' Send a request with retries and a credential refresh on 401
#'
#' @param raw If TRUE return the response without turning an error status into
#'   a condition (the polling loop needs to see 202 and 429).
#' @noRd
.sf_api_request_with_refresh <- function(con, method, url, body = NULL,
                                         request_id = NULL, deadline = NULL,
                                         raw = FALSE, request_timeout = NULL) {
  .refresh_token_if_stale(con)

  resp <- .sf_send(con, method, url, body, request_id, deadline,
                   request_timeout = request_timeout)

  if (httr2::resp_status(resp) == 401L) {
    refreshed <- .try_refresh_token(con)
    if (refreshed) {
      # The 401 was an authentication failure, so the statement did not run:
      # the resend is a first attempt, with no retry=true.
      resp <- .sf_send(con, method, url, body, request_id, deadline,
                       request_timeout = request_timeout)
    }
  }

  if (raw) return(resp)
  .handle_response(resp, url, con)
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

  # External browser SSO exposes no raw token to re-derive, so re-request
  # credentials for the same identity instead. snowflakeauth renews from its
  # own session/ID-token cache where it can, and only reopens the browser when
  # the cached ID token has itself expired -- so this is cheap in the common
  # case and still recovers in the uncommon one. Without it a 401 mid-session
  # was terminal for this auth type.
  if (identical(auth$type, "externalbrowser")) {
    if (is.null(auth$params)) {
      return(FALSE)
    }
    return(tryCatch({
      new_headers <- snowflakeauth::snowflake_credentials(auth$params)
      old_headers <- con@.state$headers %||% auth$headers
      changed <- !identical(as.list(new_headers), as.list(old_headers))
      if (changed) {
        con@.state$headers <- new_headers
      }
      changed
    }, error = function(e) FALSE))
  }

  FALSE
}


# ---------------------------------------------------------------------------
# Internal HTTP helper
# ---------------------------------------------------------------------------

#' Perform one SQL API request, with no retry
#'
#' Retrying is `.sf_send()`'s job: it has to know whether the request carries a
#' request id, and httr2's own `req_retry()` resends without one.
#' @param timeout Seconds for this attempt; defaults to `skiLift.timeout`.
#' @noRd
.sf_api_request_raw <- function(con, method, url, body = NULL, timeout = NULL) {
  auth <- con@.auth
  # .state$token is the live copy -- see .try_refresh_token() -- and is only
  # unset for connections predating this cache (e.g. hand-built in tests).
  token <- con@.state$token %||% auth$token
  token_type <- auth$token_type %||% "KEYPAIR_JWT"

  # Most auth methods yield a raw token that we wrap as a Bearer credential.
  # Some -- external browser SSO, workload identity -- use the
  # `Snowflake Token="..."` scheme instead, and supply a completed header set
  # rather than a token. Prefer that when present.
  # .state$headers is the live copy, same reason as .state$token above.
  live_headers <- con@.state$headers %||% auth$headers
  auth_headers <- if (!is.null(live_headers)) {
    as.list(live_headers)
  } else {
    list(
      "Authorization" = paste("Bearer", token),
      "X-Snowflake-Authorization-Token-Type" = token_type
    )
  }

  req <- httr2::request(url) |>
    httr2::req_headers(
      !!!auth_headers,
      "Content-Type"  = "application/json",
      "Accept"        = "application/json",
      "User-Agent"    = sf_user_agent()
    ) |>
    httr2::req_timeout(timeout %||% getOption("skiLift.timeout", 600L))

  if (!is.null(body)) {
    req <- req |> httr2::req_body_json(body, auto_unbox = TRUE)
  }

  if (toupper(method) == "GET") {
    req <- req |> httr2::req_method("GET")
  }

  req |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform()
}

#' Turn a response into its parsed body, or into a classed error
#'
#' 200 and 202 return the body, with the status kept as the `http_status`
#' attribute (202 means the statement is still running; see `sf_api_submit()`).
#' Any other status raises an error of class `skilift_error_http` and a more
#' specific one where there is one (see `.sf_http_class()`), carrying `status`,
#' `code`, `sql_state`, `statement_handle`, `request_id` and `attempts`.
#' The message text is Snowflake's, followed by one hint where we have one.
#' @noRd
.handle_response <- function(resp, url, con = NULL) {
  status <- httr2::resp_status(resp)

  if (status %in% c(200L, 202L)) {
    body <- .parse_json_body(resp)
    attr(body, "http_status") <- status
    return(body)
  }

  meta <- attr(resp, "skilift")
  auth_type <- if (!is.null(con)) con@.auth$type
  err_body <- tryCatch(.parse_json_body(resp), error = function(e) list())
  sf_code <- err_body$code %||% as.character(status)
  sf_msg  <- err_body$message %||%
             tryCatch(httr2::resp_body_string(resp), error = function(e) "(no body)")

  .sf_abort(
    c(
      "x" = "Snowflake SQL API error (HTTP {status}, code {sf_code}).",
      "i" = "{sf_msg}",
      .sf_hints(status, auth_type, message = sf_msg)
    ),
    class = .sf_http_class(status),
    status = status, code = sf_code,
    sql_state = err_body$sqlState,
    statement_handle = err_body$statementHandle,
    request_id = meta$request_id, attempts = meta$attempts,
    call = NULL
  )
}

#' Parse a JSON response body, using RcppSimdJson when available
#'
#' Uses fparse with default simplification for maximum speed.  Downstream
#' parsers (sf_parse_metadata, .json_data_to_df) handle both the simplified
#' structures (data.frame/matrix) from fparse and the nested-list structures
#' from httr2::resp_body_json().
#' @noRd
.parse_json_body <- function(resp) {
  use_simd <- isTRUE(getOption("skiLift.use_simdjson", TRUE))
  if (use_simd && requireNamespace("RcppSimdJson", quietly = TRUE)) {
    raw_bytes <- httr2::resp_body_raw(resp)
    return(RcppSimdJson::fparse(rawToChar(raw_bytes)))
  }
  httr2::resp_body_json(resp)
}
