#' Errors raised by skiLift
#'
#' Errors that come from Snowflake, or from failing to reach it, have a class
#' and carry the facts as data, so code can tell an authentication failure from
#' a SQL error from a timeout without reading the message.
#'
#' @section Classes:
#' Every error has class `skilift_error`. More specific classes come first.
#'
#' * `skilift_error_config`: a problem found locally, before any request
#'   (bad arguments, a row too large to send, transactions, which skiLift does
#'   not support).
#' * `skilift_error_http`: Snowflake answered with an error status. Also one of:
#'   * `skilift_error_auth`: 401, after the credentials were refreshed once.
#'   * `skilift_error_forbidden`: 403.
#'   * `skilift_error_sql`: 422, the SQL did not compile or run.
#' * `skilift_error_timeout`: a statement did not finish within
#'   `options(skiLift.timeout)` (it is then cancelled), or a request timed out.
#' * `skilift_error_retry`: a temporary failure (429, 500, 502, 503, 504, or no
#'   response at all) lasted through every retry. The first cause is in `parent`.
#' * `skilift_error_write`: a write with `dbWriteTable()` or `dbAppendTable()`
#'   stopped part-way.
#'
#' @section Fields:
#' These are stored on the condition, and are `NULL` when not known.
#'
#' * `status`: the HTTP status.
#' * `code`: Snowflake's error code, for example `"002003"`.
#' * `sql_state`: the SQL state, for example `"42S02"`.
#' * `statement_handle`: the statement's id, which is its query id in Snowflake's
#'   Query History.
#' * `request_id`: the id skiLift sent with the request. Quote it when asking
#'   Snowflake support about a statement.
#' * `attempts`: how many times the request was sent.
#'
#' An error of class `skilift_error_write` also has `outcome`, `rows_committed`,
#' `batches_committed`, `total_rows` and `failed_batch` (`index`, `first_row`,
#' `n_rows`). `outcome` says what is true of the failed batch:
#'
#' * `"failed"`: Snowflake ran it and it failed, so it is not in the table.
#' * `"not_executed"`: it was rejected before it ran, so it is not in the table.
#' * `"unknown"`: the retries ran out or the request timed out, so it **may** have
#'   been committed. Look up `request_id` in Snowflake's Query History before
#'   writing the rows again, or they may be duplicated.
#'
#' @section Retries:
#' A statement is sent with a `requestId`. If the request has to be sent again,
#' the same id is sent with `retry=true`, and Snowflake does not run the
#' statement a second time if it already ran. So a retried `INSERT` does not
#' write its rows twice. Only temporary failures are retried; a SQL error, 400,
#' 401 (after one credential refresh) and 403 are not. See
#' [skiLift-options] for the limits.
#'
#' @examples
#' \dontrun{
#' result <- tryCatch(
#'   dbGetQuery(con, "SELECT * FROM no_such_table"),
#'   skilift_error_sql = function(e) {
#'     message("Snowflake says ", e$code, " (SQL state ", e$sql_state, ")")
#'     NULL
#'   },
#'   skilift_error_timeout = function(e) {
#'     message("Look up ", e$statement_handle, " in Query History")
#'     NULL
#'   }
#' )
#' }
#' @name skilift-errors
#' @aliases skilift_error
NULL

#' skiLift options for timeouts, retries and batch writes
#'
#' Set with [options()]. Each is read when it is used, so changing one takes
#' effect for the next statement.
#'
#' @section Options:
#' * `skiLift.timeout` (default `600`): seconds a statement may take, measured
#'   from when it is submitted. It is passed to Snowflake as the statement
#'   timeout, and it also bounds skiLift's own waiting and retrying. A statement
#'   still running at the deadline is cancelled and raises an error of class
#'   `skilift_error_timeout`. This is one limit for the whole statement, not one
#'   per attempt.
#' * `skiLift.request_timeout` (default `90`): seconds one request may wait for
#'   Snowflake to answer before it is abandoned and sent again. Snowflake answers
#'   a statement within about 45 seconds (with the result, or with a handle if it
#'   is still running) and a status check within seconds, so a request silent for
#'   90 seconds has hung. Without this limit a hung request waited out all of
#'   `skiLift.timeout`. The resend carries the same request id, so it does not run
#'   the statement twice. Downloading a large result partition is not limited by
#'   it.
#' * `skiLift.retry_max` (default `3`): how many times a request is sent, in all,
#'   when it fails in a way that is safe to retry. The wait between attempts
#'   grows (about 0.5, 1, 2, ... seconds, with some randomness) and is at least
#'   the `Retry-After` Snowflake asks for.
#' * `skiLift.insert_batch_size` (default `16384`): the most rows in one `INSERT`
#'   statement when writing a data frame.
#' * `skiLift.insert_batch_bytes` (default `1e6`): the most bytes of SQL in one
#'   `INSERT`. A batch ends at whichever of the two limits comes first. Snowflake
#'   advises keeping a request near 1 MB; wide rows or long text reach this limit
#'   long before the row limit. A single row larger than this cannot be sent and
#'   is an error.
#'
#' @section Waiting for long statements:
#' Snowflake answers a statement that runs for more than 45 seconds with a
#' handle instead of a result. skiLift then checks again, waiting longer each time
#' (from 0.5 up to 5 seconds), until the statement has finished, so a slow query
#' or a slow `INSERT` returns only when it is done. Pressing Esc while waiting
#' cancels the statement in Snowflake so it does not keep running.
#'
#' @name skiLift-options
NULL
