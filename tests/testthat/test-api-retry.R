# Retries that cannot run a statement twice
# =============================================================================
# Snowflake runs a statement once if it is resent with the same requestId and
# retry=true. Without that, a 503 or a dropped connection AFTER the statement
# ran would run it again: an INSERT batch would write its rows twice.

test_that("every statement carries a requestId that looks like a version 4 UUID", {
  con <- .api_con()
  m <- .mock_sequence(list(.json_resp(200, list(statementHandle = "h"))))
  httr2::local_mocked_responses(m$mock)
  sf_api_submit(con, "SELECT 1")
  id <- .request_id_of(m$urls())
  expect_match(id, "^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$")
})

test_that("a 503 is resent with the SAME requestId and retry=true on the resend only", {
  con <- .api_con()
  sleeps <- .record_sleeps()
  m <- .mock_sequence(list(.json_resp(503), .json_resp(200, list(statementHandle = "h"))))
  httr2::local_mocked_responses(m$mock)
  sf_api_submit(con, "INSERT INTO t VALUES (1)")
  urls <- m$urls()
  expect_length(urls, 2L)
  expect_equal(.request_id_of(urls[1]), .request_id_of(urls[2]))
  expect_false(grepl("retry=true", urls[1], fixed = TRUE))
  expect_true(grepl("retry=true", urls[2], fixed = TRUE))
  expect_length(sleeps(), 1L)
})

test_that("a dropped connection is retried the same way", {
  con <- .api_con()
  .record_sleeps()
  m <- .mock_sequence(list(.network_failure, .json_resp(200, list(statementHandle = "h"))))
  httr2::local_mocked_responses(m$mock)
  sf_api_submit(con, "INSERT INTO t VALUES (1)")
  urls <- m$urls()
  expect_length(urls, 2L)
  expect_equal(.request_id_of(urls[1]), .request_id_of(urls[2]))
  expect_true(grepl("retry=true", urls[2], fixed = TRUE))
})

test_that("500, 502, 504 and 429 are retried", {
  for (status in c(500L, 502L, 504L, 429L)) {
    con <- .api_con()
    .record_sleeps()
    m <- .mock_sequence(list(.json_resp(status), .json_resp(200, list(statementHandle = "h"))))
    httr2::local_mocked_responses(m$mock)
    sf_api_submit(con, "SELECT 1")
    expect_length(m$urls(), 2L)
  }
})

test_that("a SQL error (422), a 403 and a 400 are not retried", {
  for (status in c(422L, 403L, 400L)) {
    con <- .api_con()
    .record_sleeps()
    m <- .mock_sequence(list(.json_resp(status, list(code = "1", message = "no"))))
    httr2::local_mocked_responses(m$mock)
    expect_error(sf_api_submit(con, "SELECT 1"), class = "skilift_error_http")
    expect_length(m$urls(), 1L)
  }
})

test_that("retries that run out raise skilift_error_retry with the facts to reconcile", {
  con <- .api_con()
  .record_sleeps()
  m <- .mock_sequence(list(.json_resp(503, list(message = "busy"))))
  httr2::local_mocked_responses(m$mock)
  cnd <- tryCatch(sf_api_submit(con, "INSERT INTO t VALUES (1)"), error = function(e) e)
  expect_s3_class(cnd, "skilift_error_retry")
  expect_s3_class(cnd, "skilift_error_http")
  expect_equal(cnd$status, 503L)
  expect_equal(cnd$attempts, 3L)
  expect_equal(cnd$request_id, .request_id_of(m$urls()[1]))
  expect_length(m$urls(), 3L)
})

test_that("a connection that never answers raises skilift_error_retry with the cause attached", {
  con <- .api_con()
  .record_sleeps()
  m <- .mock_sequence(list(.network_failure))
  httr2::local_mocked_responses(m$mock)
  cnd <- tryCatch(sf_api_submit(con, "SELECT 1"), error = function(e) e)
  expect_s3_class(cnd, "skilift_error_retry")
  expect_equal(cnd$attempts, 3L)
  expect_false(is.null(cnd$parent))
  expect_match(conditionMessage(cnd), "Snowflake API request failed")
})

test_that("a request timeout is classed as a timeout as well as a retry failure", {
  con <- .api_con()
  .record_sleeps()
  m <- .mock_sequence(list(function(req) stop("Timeout was reached: Operation timed out", call. = FALSE)))
  httr2::local_mocked_responses(m$mock)
  expect_error(sf_api_submit(con, "SELECT 1"), class = "skilift_error_timeout")
})

test_that("two statements get different requestIds", {
  con <- .api_con()
  m <- .mock_sequence(list(.json_resp(200, list(statementHandle = "h"))))
  httr2::local_mocked_responses(m$mock)
  sf_api_submit(con, "SELECT 1")
  sf_api_submit(con, "SELECT 2")
  ids <- vapply(m$urls(), .request_id_of, "")
  expect_false(ids[1] == ids[2])
})

test_that("the wait before a resend honours Retry-After and is never shorter", {
  con <- .api_con()
  sleeps <- .record_sleeps()
  m <- .mock_sequence(list(.json_resp(429, headers = list(`Retry-After` = "7")),
                           .json_resp(200, list(statementHandle = "h"))))
  httr2::local_mocked_responses(m$mock)
  sf_api_submit(con, "SELECT 1")
  expect_gte(sleeps()[1], 7)
})

test_that("an async submit keeps its requestId alongside async=true", {
  con <- .api_con()
  m <- .mock_sequence(list(.json_resp(202, list(statementHandle = "h"))))
  httr2::local_mocked_responses(m$mock)
  body <- sf_api_submit(con, "SELECT 1", async = TRUE)
  expect_equal(body$statementHandle, "h")
  expect_length(m$urls(), 1L)
  expect_match(m$urls(), "\\?async=true&requestId=")
})

test_that("a 401 is not counted as a retry: the resend after a refresh is a first attempt", {
  con <- .api_con()
  con@.state$token <- "old"
  m <- .mock_sequence(list(.json_resp(401, list(message = "expired")),
                           .json_resp(200, list(statementHandle = "h"))))
  httr2::local_mocked_responses(m$mock)
  testthat::local_mocked_bindings(
    .try_refresh_token = function(con) { con@.state$token <- "new"; TRUE },
    .package = "skiLift"
  )
  sf_api_submit(con, "SELECT 1")
  expect_length(m$urls(), 2L)
  expect_false(grepl("retry=true", m$urls()[2], fixed = TRUE))
})

test_that("backoff does not touch the user's random seed", {
  set.seed(42)
  expected <- runif(1)
  set.seed(42)
  for (i in 1:5) .sf_backoff(i)
  .new_request_id()
  expect_equal(runif(1), expected)
})

test_that("an overall deadline stops the retrying", {
  con <- .api_con()
  withr::local_options(skiLift.timeout = 1L, skiLift.retry_max = 50L)
  testthat::local_mocked_bindings(
    .sf_sleep = function(seconds) Sys.sleep(0.6),
    .package = "skiLift"
  )
  m <- .mock_sequence(list(.json_resp(503)))
  httr2::local_mocked_responses(m$mock)
  expect_error(sf_api_submit(con, "SELECT 1"), class = "skilift_error")
  expect_lt(length(m$urls()), 10L)
})

# ---------------------------------------------------------------------------
# A request that hangs is abandoned quickly and resent, not waited out
# ---------------------------------------------------------------------------
# Found live on 29 Oct 2026: a statement POST sat with "0 bytes received" for the
# whole 600 s request timeout (and in earlier runs for 16 minutes). Snowflake
# answers a POST within ~45 s, so one request is now given 90 s, and the resend
# is safe because it carries the same request id.

test_that("a statement POST is given skiLift.request_timeout, not the whole statement timeout", {
  con <- .api_con()
  m <- .mock_sequence(list(.json_resp(200, list(statementHandle = "h"))))
  httr2::local_mocked_responses(m$mock)
  sf_api_submit(con, "SELECT 1")
  expect_equal(m$timeouts(), 90000)
})

test_that("a request that hangs is abandoned and resent with the same requestId and retry=true", {
  con <- .api_con()
  .record_sleeps()
  m <- .mock_sequence(list(
    function(req) stop("Timeout was reached: Operation timed out after 90001 milliseconds with 0 bytes received", call. = FALSE),
    .json_resp(200, list(statementHandle = "h"))
  ))
  httr2::local_mocked_responses(m$mock)
  sf_api_submit(con, "CALL SLOW()")
  urls <- m$urls()
  expect_length(urls, 2L)
  expect_equal(.request_id_of(urls[1]), .request_id_of(urls[2]))
  expect_true(grepl("retry=true", urls[2], fixed = TRUE))
})

test_that("skiLift.timeout still caps a request when it is shorter", {
  con <- .api_con()
  withr::local_options(skiLift.timeout = 30L)
  m <- .mock_sequence(list(.json_resp(200, list(statementHandle = "h"))))
  httr2::local_mocked_responses(m$mock)
  sf_api_submit(con, "SELECT 1")
  expect_equal(m$timeouts(), 30000, tolerance = 0.01)   # a few ms of the deadline are already used
})

test_that("a status check is limited too, but a partition download is not", {
  con <- .api_con()
  .record_sleeps()
  m <- .mock_sequence(list(.json_resp(202, list(statementHandle = "h")),
                           .json_resp(200, list(statementHandle = "h"))))
  httr2::local_mocked_responses(m$mock)
  sf_api_submit(con, "SELECT SLOW()")
  expect_equal(m$timeouts(), c(90000, 90000))      # the POST and the poll

  m2 <- .mock_sequence(list(.json_resp(200, list(data = list()))))
  httr2::local_mocked_responses(m2$mock)
  sf_api_fetch_partition(con, "h", 3L)
  expect_equal(m2$timeouts(), 600000, tolerance = 0.01)   # a large download may take a while
})
