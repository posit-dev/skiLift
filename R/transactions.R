# Transactions are not supported
# =============================================================================
# skiLift sends every statement to the Snowflake SQL API on its own, and
# Snowflake commits each one as it runs. A BEGIN sent that way has nothing to
# hold open, so there is no way to group later statements into a transaction.
#
# Until 29 Oct 2026 a session-based connection (options(skiLift.use_session =
# TRUE)) accepted dbBegin(), dbCommit() and dbRollback(), but sent only the
# BEGIN, COMMIT and ROLLBACK through the session. The statements in between
# still went through the SQL API and committed at once, so a rolled-back
# transaction left its rows behind, and dbWithTransaction() was not atomic. The
# session request itself was also failing (no requestId; a malformed
# Authorization header), so the transaction methods could not even start a
# transaction. They now refuse, loudly, rather than appear to work.
# Routing every statement through the session would need the internal
# protocol's different result format (booleans as 1, encrypted result chunks)
# and is not done: see backlog DB-32.

.txn_not_supported <- function(call = rlang::caller_env()) {
  .sf_abort(
    c(
      "x" = "Transactions are not supported by skiLift.",
      "i" = "Each statement is sent to the Snowflake SQL API on its own, and Snowflake commits it as soon as it runs. A {.code BEGIN} cannot hold later statements open.",
      "i" = "To make changes that must all succeed or all fail, send them as one Snowflake Scripting block: {.code dbExecute(con, \"EXECUTE IMMEDIATE $$ BEGIN BEGIN TRANSACTION; ...; COMMIT; END $$\")}.",
      "i" = "See {.code ?dbBegin}."
    ),
    class = "skilift_error_config",
    call = call
  )
}

#' @rdname SnowflakeConnection-class
#' @section Transactions:
#' skiLift does not support transactions. `dbBegin()`, `dbCommit()`,
#' `dbRollback()` and `dbWithTransaction()` raise an error of class
#' `skilift_error_config`. Each statement is sent to the Snowflake SQL API on its
#' own and committed as it runs, so there is nothing for a `BEGIN` to hold open.
#' To make changes that must all succeed or all fail, send them as one
#' Snowflake Scripting block:
#'
#' ```
#' dbExecute(con, "EXECUTE IMMEDIATE $$
#'   BEGIN
#'     BEGIN TRANSACTION;
#'     INSERT INTO t VALUES (1);
#'     INSERT INTO t VALUES (2);
#'     COMMIT;
#'   END
#' $$")
#' ```
#'
#' If a statement in the block fails, the block fails and nothing is committed.
#' Writes with `dbWriteTable()` and `dbAppendTable()` are sent in batches, each
#' committed on its own, so they are not atomic either; see `?dbWriteTable`.
#' @export
setMethod("dbBegin", "SnowflakeConnection", function(conn, ...) {
  .check_valid(conn)
  .txn_not_supported()
})

#' @rdname SnowflakeConnection-class
#' @export
setMethod("dbCommit", "SnowflakeConnection", function(conn, ...) {
  .check_valid(conn)
  .txn_not_supported()
})

#' @rdname SnowflakeConnection-class
#' @export
setMethod("dbRollback", "SnowflakeConnection", function(conn, ...) {
  .check_valid(conn)
  .txn_not_supported()
})

#' @rdname SnowflakeConnection-class
#' @param code Code to execute within the transaction.
#' @export
setMethod("dbWithTransaction", "SnowflakeConnection",
  function(conn, code, ...) {
    .check_valid(conn)
    .txn_not_supported()
  }
)
