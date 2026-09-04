# Transaction Support
# =============================================================================
# When a session-based connection is active, transactions are supported via
# the internal /queries/v1/ endpoint (BEGIN/COMMIT/ROLLBACK).
# Without a session, the SQL API v2 is stateless per-request and transactions
# are rejected with an informative error.

.txn_not_supported <- function() {
  cli_abort(c(
    "Transactions require a session-based connection.",
    "i" = "Set {.code options(skiLift.use_session = TRUE)} before connecting.",
    "i" = "Without a session, the SQL API v2 is stateless per-request."
  ))
}

#' @rdname SnowflakeConnection-class
#' @export
setMethod("dbBegin", "SnowflakeConnection", function(conn, ...) {
  .check_valid(conn)
  if (!.has_session(conn)) .txn_not_supported()
  if (conn@.state$in_transaction) {
    cli_abort("A transaction is already active on this connection.")
  }
  sf_internal_execute(conn, "BEGIN")
  conn@.state$in_transaction <- TRUE
  invisible(TRUE)
})

#' @rdname SnowflakeConnection-class
#' @export
setMethod("dbCommit", "SnowflakeConnection", function(conn, ...) {
  .check_valid(conn)
  if (!.has_session(conn)) .txn_not_supported()
  if (!conn@.state$in_transaction) {
    cli_abort("No active transaction to commit.")
  }
  sf_internal_execute(conn, "COMMIT")
  conn@.state$in_transaction <- FALSE
  invisible(TRUE)
})

#' @rdname SnowflakeConnection-class
#' @export
setMethod("dbRollback", "SnowflakeConnection", function(conn, ...) {
  .check_valid(conn)
  if (!.has_session(conn)) .txn_not_supported()
  if (!conn@.state$in_transaction) {
    cli_abort("No active transaction to roll back.")
  }
  sf_internal_execute(conn, "ROLLBACK")
  conn@.state$in_transaction <- FALSE
  invisible(TRUE)
})

#' @rdname SnowflakeConnection-class
#' @param code Code to execute within the transaction.
#' @export
setMethod("dbWithTransaction", "SnowflakeConnection",
  function(conn, code, ...) {
    .check_valid(conn)
    if (!.has_session(conn)) .txn_not_supported()

    dbBegin(conn)
    tryCatch(
      {
        result <- force(code)
        dbCommit(conn)
        result
      },
      error = function(e) {
        tryCatch(dbRollback(conn), error = function(e2) NULL)
        stop(e)
      }
    )
  }
)
