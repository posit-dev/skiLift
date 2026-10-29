# Statements that outlast Snowflake's 45-second synchronous window
# =============================================================================
# Snowflake answers a statement that runs longer than 45 seconds with HTTP 202
# and a handle, not a result. Until 29 Oct 2026 skiLift took that 202 for the
# result: a long query came back as a data frame with no rows and no columns,
# and a long INSERT reported success before it had finished. Found live with
# SELECT SYSTEM$WAIT(50).

.result_body <- function() {
  list(statementHandle = "h1", code = "090001",
       resultSetMetaData = list(numRows = 1L, rowType = list()),
       data = list(list("1")))
}
.pending_body <- function() {
  list(statementHandle = "h1", code = "333334",
       message = "Asynchronous execution in progress.",
       statementStatusUrl = "/api/v2/statements/h1")
}

test_that("a 202 is polled until the result arrives, and the result is returned", {
  con <- .api_con()
  sleeps <- .record_sleeps()
  m <- .mock_sequence(list(
    .json_resp(202, .pending_body()),   # the POST
    .json_resp(202, .pending_body()),   # poll 1
    .json_resp(202, .pending_body()),   # poll 2
    .json_resp(200, .result_body())     # poll 3
  ))
  httr2::local_mocked_responses(m$mock)
  body <- sf_api_submit(con, "SELECT SYSTEM$WAIT(50)")
  expect_equal(body$statementHandle, "h1")
  expect_equal(attr(body, "http_status"), 200L)
  reqs <- m$requests()
  expect_length(reqs, 4L)
  expect_equal(vapply(reqs, `[[`, "", "method"), c("POST", "GET", "GET", "GET"))
  expect_true(all(grepl("/api/v2/statements/h1$", vapply(reqs[-1L], `[[`, "", "url"))))
  # Waits grow from 0.5 s and stay at or under 5 s
  expect_length(sleeps(), 3L)
  expect_equal(sleeps()[1], 0.5)
  expect_true(all(diff(sleeps()) >= 0) && max(sleeps()) <= 5)
})

test_that("a statement that finishes within the window is not polled", {
  con <- .api_con()
  .record_sleeps()
  m <- .mock_sequence(list(.json_resp(200, .result_body())))
  httr2::local_mocked_responses(m$mock)
  sf_api_submit(con, "SELECT 1")
  expect_length(m$requests(), 1L)
})

test_that("async = TRUE returns the handle at once and never polls", {
  con <- .api_con()
  m <- .mock_sequence(list(.json_resp(202, .pending_body())))
  httr2::local_mocked_responses(m$mock)
  body <- sf_api_submit(con, "CALL SLOW()", async = TRUE)
  expect_equal(body$statementHandle, "h1")
  expect_length(m$requests(), 1L)
})

test_that("a statement that fails while we wait raises the SQL error, and is not cancelled", {
  con <- .api_con()
  .record_sleeps()
  cancelled <- character()
  testthat::local_mocked_bindings(
    sf_api_cancel = function(con, handle) cancelled <<- c(cancelled, handle),
    .package = "skiLift"
  )
  m <- .mock_sequence(list(
    .json_resp(202, .pending_body()),
    .json_resp(202, .pending_body()),
    .json_resp(422, list(code = "100038", message = "Numeric value 'x' is not recognized",
                         sqlState = "22018", statementHandle = "h1"))
  ))
  httr2::local_mocked_responses(m$mock)
  cnd <- tryCatch(sf_api_submit(con, "SELECT 'x'::INT"), error = function(e) e)
  expect_s3_class(cnd, "skilift_error_sql")
  expect_equal(cnd$sql_state, "22018")
  expect_equal(cnd$statement_handle, "h1")
  expect_length(cancelled, 0L)
})

test_that("429 while polling means 'still running', not failure", {
  con <- .api_con()
  .record_sleeps()
  m <- .mock_sequence(list(
    .json_resp(202, .pending_body()),
    .json_resp(429), .json_resp(429), .json_resp(429), .json_resp(429),
    .json_resp(200, .result_body())
  ))
  httr2::local_mocked_responses(m$mock)
  body <- sf_api_submit(con, "SELECT 1")
  expect_equal(attr(body, "http_status"), 200L)
})

test_that("passing the deadline cancels the statement and raises a timeout with its handle", {
  con <- .api_con()
  withr::local_options(skiLift.timeout = 2L)
  cancelled <- character()
  testthat::local_mocked_bindings(
    sf_api_cancel = function(con, handle) cancelled <<- c(cancelled, handle),
    # Each wait uses up the time that is left
    .sf_sleep = function(seconds) Sys.sleep(1.1),
    .package = "skiLift"
  )
  m <- .mock_sequence(list(.json_resp(202, .pending_body())))
  httr2::local_mocked_responses(m$mock)
  cnd <- tryCatch(sf_api_submit(con, "SELECT SYSTEM$WAIT(500)"), error = function(e) e)
  expect_s3_class(cnd, "skilift_error_timeout")
  expect_equal(cnd$statement_handle, "h1")
  expect_false(is.null(cnd$request_id))
  expect_equal(cancelled, "h1")
  expect_match(conditionMessage(cnd), "did not finish within 2 seconds")
})

test_that("any other way out of the wait cancels the statement (an interrupt takes this path)", {
  con <- .api_con()
  cancelled <- character()
  testthat::local_mocked_bindings(
    sf_api_cancel = function(con, handle) cancelled <<- c(cancelled, handle),
    .sf_sleep = function(seconds) stop("simulated interrupt"),
    .package = "skiLift"
  )
  m <- .mock_sequence(list(.json_resp(202, .pending_body())))
  httr2::local_mocked_responses(m$mock)
  expect_error(sf_api_submit(con, "SELECT SYSTEM$WAIT(500)"), "simulated interrupt")
  expect_equal(cancelled, "h1")
})

test_that("a partition that comes back 202 is polled, and the statement is not cancelled", {
  con <- .api_con()
  .record_sleeps()
  cancelled <- character()
  testthat::local_mocked_bindings(
    sf_api_cancel = function(con, handle) cancelled <<- c(cancelled, handle),
    .package = "skiLift"
  )
  m <- .mock_sequence(list(.json_resp(202, .pending_body()),
                           .json_resp(202, .pending_body()),
                           .json_resp(200, list(data = list(list("7"))))))
  httr2::local_mocked_responses(m$mock)
  part <- sf_api_fetch_partition(con, "h1", 2L)
  expect_equal(part$data[[1]][[1]], "7")
  expect_length(m$requests(), 3L)
  expect_true(all(grepl("partition=2", m$urls())))
  expect_length(cancelled, 0L)
})
