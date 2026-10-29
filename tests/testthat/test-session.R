# Tests for session management and transaction support
# =============================================================================

.mock_session_con <- function(has_session = FALSE) {
  state <- .new_conn_state()
  if (has_session) {
    state$session <- list(
      token            = "session-tok-123",
      master_token     = "master-tok-456",
      session_id       = 12345L,
      validity_seconds = 14400L,
      created_at       = Sys.time()
    )
  }

  new("SnowflakeConnection",
    account = "test", user = "user", database = "db", schema = "sch",
    warehouse = "wh", role = "role",
    .auth = list(token = "tok", token_type = "KEYPAIR_JWT"),
    .state = state
  )
}


# ---------------------------------------------------------------------------
# Session helpers
# ---------------------------------------------------------------------------

test_that(".has_session returns FALSE for stateless connection", {
  con <- .mock_session_con(has_session = FALSE)
  expect_false(.has_session(con))
})

test_that(".has_session returns TRUE for session-based connection", {
  con <- .mock_session_con(has_session = TRUE)
  expect_true(.has_session(con))
})

test_that(".session_needs_renewal returns FALSE for fresh session", {
  session <- list(
    token            = "tok",
    validity_seconds = 14400L,
    created_at       = Sys.time()
  )
  expect_false(.session_needs_renewal(session))
})

test_that(".session_needs_renewal returns TRUE for expired session", {
  session <- list(
    token            = "tok",
    validity_seconds = 100L,
    created_at       = Sys.time() - 80
  )
  expect_true(.session_needs_renewal(session))
})

test_that(".session_needs_renewal returns FALSE for NULL session", {
  expect_false(.session_needs_renewal(NULL))
})


# ---------------------------------------------------------------------------
# Transaction methods: not supported, on any connection
# ---------------------------------------------------------------------------
# A session-based connection used to accept dbBegin() but send only the BEGIN,
# COMMIT and ROLLBACK through the session; the statements in between still went
# through the stateless SQL API and committed at once. dbRollback() then left
# the rows in the table. Found live on 29 Oct 2026. These methods now refuse.

test_that("the transaction methods refuse, with a classed error, without a session", {
  con <- .mock_session_con(has_session = FALSE)
  for (f in list(dbBegin, dbCommit, dbRollback)) {
    expect_error(f(con), class = "skilift_error_config")
  }
  expect_error(dbWithTransaction(con, TRUE), class = "skilift_error_config")
})

test_that("they refuse with a session too: a session does not make them atomic", {
  con <- .mock_session_con(has_session = TRUE)
  for (f in list(dbBegin, dbCommit, dbRollback)) {
    expect_error(f(con), "Transactions are not supported")
  }
  expect_error(dbWithTransaction(con, TRUE), "Transactions are not supported")
})

test_that("the message names the alternative: one Snowflake Scripting block", {
  con <- .mock_session_con(has_session = TRUE)
  expect_error(dbBegin(con), "EXECUTE IMMEDIATE")
})

test_that("a refused dbWithTransaction never runs its code", {
  con <- .mock_session_con(has_session = TRUE)
  ran <- FALSE
  expect_error(dbWithTransaction(con, { ran <- TRUE }), class = "skilift_error_config")
  expect_false(ran)
})

test_that("refusing sends nothing to Snowflake", {
  con <- .mock_session_con(has_session = TRUE)
  httr2::local_mocked_responses(function(req) stop("no request should be made"))
  expect_error(dbBegin(con), class = "skilift_error_config")
  expect_error(dbRollback(con), class = "skilift_error_config")
})


# ---------------------------------------------------------------------------
# dbDisconnect with session
# ---------------------------------------------------------------------------

test_that("dbDisconnect works and invalidates connection (no session)", {
  con <- .mock_session_con(has_session = FALSE)
  dbDisconnect(con)
  expect_false(dbIsValid(con))
})

test_that("dbDisconnect with session clears session state", {
  con <- .mock_session_con(has_session = TRUE)
  expect_true(.has_session(con))

  # Disconnect -- session delete will fail (no real server) but should not error
  dbDisconnect(con)
  expect_false(dbIsValid(con))
  expect_null(con@.state$session)
})
