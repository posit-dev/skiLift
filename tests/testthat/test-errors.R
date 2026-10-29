# Errors users can catch by class
# =============================================================================

.err_for <- function(status, body = list(), con = .api_con(), headers = list()) {
  .record_sleeps(parent.frame())
  m <- .mock_sequence(list(.json_resp(status, body, headers)))
  httr2::local_mocked_responses(m$mock, env = parent.frame())
  tryCatch(sf_api_submit(con, "SELECT 1"), error = function(e) e)
}

test_that("401 after the credentials could not be refreshed is skilift_error_auth", {
  cnd <- .err_for(401, list(code = "390303", message = "Invalid OAuth access token."))
  expect_s3_class(cnd, "skilift_error_auth")
  expect_s3_class(cnd, "skilift_error_http")
  expect_s3_class(cnd, "skilift_error")
  expect_equal(cnd$status, 401L)
  expect_equal(cnd$code, "390303")
})

test_that("403 is skilift_error_forbidden, with the network-policy hint", {
  cnd <- .err_for(403, list(code = "390422", message = "IP 1.2.3.4 is not allowed to access Snowflake."))
  expect_s3_class(cnd, "skilift_error_forbidden")
  expect_match(conditionMessage(cnd), "network policy")
})

test_that("422 is skilift_error_sql and carries Snowflake's facts as data", {
  cnd <- .err_for(422, list(code = "002003", sqlState = "42S02", statementHandle = "01abc",
                            message = "SQL compilation error: Object 'T' does not exist or not authorized."))
  expect_s3_class(cnd, "skilift_error_sql")
  expect_equal(cnd$status, 422L)
  expect_equal(cnd$code, "002003")
  expect_equal(cnd$sql_state, "42S02")
  expect_equal(cnd$statement_handle, "01abc")
  expect_match(cnd$request_id, "^[0-9a-f-]{36}$")
  expect_equal(cnd$attempts, 1L)
})

test_that("the original message text is kept; the hint follows it", {
  cnd <- .err_for(422, list(code = "002003",
                            message = "Object 'T' does not exist or not authorized."))
  msg <- conditionMessage(cnd)
  expect_match(msg, "Snowflake SQL API error (HTTP 422, code 002003).", fixed = TRUE)
  expect_match(msg, "Object 'T' does not exist or not authorized.", fixed = TRUE)
  expect_match(msg, "missing privilege", fixed = TRUE)
})

test_that("a key-pair 401 points at the public key and the clock; a token 401 at expiry", {
  jwt <- .api_con(type = "jwt")
  expect_match(conditionMessage(.err_for(401, list(message = "JWT token is invalid"), con = jwt)),
               "RSA_PUBLIC_KEY_FP")
  tok <- .api_con(type = "token")
  expect_match(conditionMessage(.err_for(401, list(message = "expired"), con = tok)),
               "expired or been revoked")
})

test_that("a server message containing braces is shown as text, not parsed", {
  cnd <- .err_for(422, list(code = "1", message = "Unexpected token {oops} at position 3"))
  expect_s3_class(cnd, "skilift_error_sql")
  expect_match(conditionMessage(cnd), "{oops}", fixed = TRUE)
})

test_that("an error body that is not JSON still gives a classed error", {
  .record_sleeps()
  m <- .mock_sequence(list(httr2::response(status_code = 422, body = charToRaw("plain text failure"))))
  httr2::local_mocked_responses(m$mock)
  cnd <- tryCatch(sf_api_submit(.api_con(), "SELECT 1"), error = function(e) e)
  expect_s3_class(cnd, "skilift_error_sql")
  expect_match(conditionMessage(cnd), "plain text failure")
})

test_that("a 408 from Snowflake is a timeout and says where to look", {
  cnd <- .err_for(408, list(code = "000630", statementHandle = "01xyz",
                            message = "Statement reached its statement or warehouse timeout"))
  expect_s3_class(cnd, "skilift_error_timeout")
  expect_equal(cnd$statement_handle, "01xyz")
  expect_match(conditionMessage(cnd), "Query History")
})

test_that("each class can be caught on its own with tryCatch", {
  out <- tryCatch(
    {
      .record_sleeps()
      m <- .mock_sequence(list(.json_resp(422, list(code = "1", sqlState = "42000", message = "bad"))))
      httr2::local_mocked_responses(m$mock)
      sf_api_submit(.api_con(), "SELECT")
    },
    skilift_error_auth = function(e) "auth",
    skilift_error_sql = function(e) e$sql_state
  )
  expect_equal(out, "42000")
})

test_that("the internal (session) protocol raises the same classes", {
  con <- .api_con()
  con@.state$session <- list(token = "s", master_token = "m", validity_seconds = 3600, created_at = Sys.time())
  .record_sleeps()
  m <- .mock_sequence(list(.json_resp(200, list(success = FALSE, message = "Syntax error", data = list(errorCode = "001003", sqlState = "42000")))))
  httr2::local_mocked_responses(m$mock)
  testthat::local_mocked_bindings(.ensure_session_fresh = function(conn) invisible(), .package = "skiLift")
  cnd <- tryCatch(sf_internal_submit(con, "SELEC"), error = function(e) e)
  expect_s3_class(cnd, "skilift_error_http")
  expect_equal(cnd$code, "001003")
  expect_equal(cnd$sql_state, "42000")
})
