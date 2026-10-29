# Batch writes: limited by bytes, honest about partial progress
# =============================================================================
# Writing is batched INSERT over the SQL API, one auto-committed statement per
# batch. Until 29 Oct 2026 batches were limited by rows only (16,384), so wide
# rows made one request many megabytes, and a failure part-way said nothing
# about how much had been written.

# Capture the SQL of each batch instead of sending it. `fail_at` makes the
# nth batch fail with `error`.
.capture_batches <- function(fail_at = NULL, error = NULL, env = parent.frame()) {
  sent <- new.env()
  sent$sql <- character()
  testthat::local_mocked_bindings(
    sf_api_submit = function(con, sql, bindings = NULL, async = FALSE) {
      sent$sql <- c(sent$sql, sql)
      if (!is.null(fail_at) && length(sent$sql) == fail_at) stop(error)
      list(statementHandle = "h")
    },
    .package = "skiLift", .env = env
  )
  sent
}

.write_con <- function() .api_con()

test_that("batches split by bytes, not just rows, and keep every row once", {
  con <- .write_con()
  withr::local_options(skiLift.insert_batch_size = 16384L, skiLift.insert_batch_bytes = 1e6)
  # 300 rows of ~10 KB each: 3 MB in all, so it cannot go in fewer than 4 requests
  df <- data.frame(id = 1:300, txt = strrep("x", 10000), stringsAsFactors = FALSE)
  sent <- .capture_batches()
  .insert_data_literal(con, '"T"', df)
  expect_gt(length(sent$sql), 3L)
  expect_true(all(nchar(sent$sql, type = "bytes") <= 1e6))
  rows_sent <- sum(vapply(sent$sql, function(s) lengths(regmatches(s, gregexpr("\\(\\d+, 'x", s))), 1L))
  expect_equal(rows_sent, 300L)
})

test_that("the row limit still applies when rows are small", {
  con <- .write_con()
  withr::local_options(skiLift.insert_batch_size = 100L, skiLift.insert_batch_bytes = 1e6)
  sent <- .capture_batches()
  .insert_data_literal(con, '"T"', data.frame(a = 1:250))
  expect_length(sent$sql, 3L)
})

test_that("the byte limit counts bytes, not characters", {
  con <- .write_con()
  withr::local_options(skiLift.insert_batch_size = 16384L, skiLift.insert_batch_bytes = 2000)
  # 100 rows x 50 characters of 3-byte text = 150 bytes of text per row
  df <- data.frame(txt = strrep("€", 50), stringsAsFactors = FALSE)[rep(1, 100), , drop = FALSE]
  sent <- .capture_batches()
  .insert_data_literal(con, '"T"', df)
  expect_true(all(nchar(sent$sql, type = "bytes") <= 2000))
  expect_gt(length(sent$sql), 5L)
})

test_that("a single row bigger than the budget fails before it is sent, saying how far it got", {
  con <- .write_con()
  withr::local_options(skiLift.insert_batch_bytes = 5000)
  df <- data.frame(id = 1:3, txt = c("a", strrep("y", 6000), "c"), stringsAsFactors = FALSE)
  sent <- .capture_batches()
  cnd <- tryCatch(.insert_data_literal(con, '"T"', df), error = function(e) e)
  expect_s3_class(cnd, "skilift_error_write")
  expect_s3_class(cnd, "skilift_error_config")
  expect_equal(cnd$outcome, "not_executed")
  expect_equal(cnd$failed_batch$first_row, 2L)
  expect_match(conditionMessage(cnd), "Row 2 is too large")
  expect_length(sent$sql, 0L)
})

test_that("a failure on the 3rd of 5 batches reports what is committed", {
  con <- .write_con()
  withr::local_options(skiLift.insert_batch_size = 10L)
  sql_error <- rlang::error_cnd(
    class = c("skilift_error_sql", "skilift_error_http", "skilift_error"),
    message = "SQL compilation error", status = 422L, code = "000904",
    statement_handle = "01h", request_id = "rid-1"
  )
  sent <- .capture_batches(fail_at = 3L, error = sql_error)
  cnd <- tryCatch(.insert_data_literal(con, '"T"', data.frame(a = 1:50)), error = function(e) e)
  expect_s3_class(cnd, "skilift_error_write")
  expect_equal(cnd$rows_committed, 20L)
  expect_equal(cnd$batches_committed, 2L)
  expect_equal(cnd$total_rows, 50L)
  expect_equal(cnd$failed_batch, list(index = 3L, first_row = 21L, n_rows = 10L))
  expect_equal(cnd$outcome, "failed")
  expect_equal(cnd$statement_handle, "01h")
  expect_equal(cnd$request_id, "rid-1")
  expect_length(sent$sql, 3L)           # it stopped; batches 4 and 5 were never sent
  expect_match(conditionMessage(cnd), "20 of 50 rows were committed")
  expect_match(conditionMessage(cnd), "not atomic")
})

test_that("the outcome says whether the failed batch may have committed", {
  con <- .write_con()
  withr::local_options(skiLift.insert_batch_size = 10L)
  cases <- list(
    list(cls = "skilift_error_sql", outcome = "failed"),
    list(cls = "skilift_error_auth", outcome = "not_executed"),
    list(cls = "skilift_error_forbidden", outcome = "not_executed"),
    list(cls = "skilift_error_retry", outcome = "unknown"),
    list(cls = "skilift_error_timeout", outcome = "unknown")
  )
  for (case in cases) {
    err <- rlang::error_cnd(class = c(case$cls, "skilift_error_http", "skilift_error"), message = "x")
    .capture_batches(fail_at = 1L, error = err)
    cnd <- tryCatch(.insert_data_literal(con, '"T"', data.frame(a = 1:5)), error = function(e) e)
    expect_equal(cnd$outcome, case$outcome, info = case$cls)
  }
  # and for "unknown" the message warns about duplicates
  err <- rlang::error_cnd(class = c("skilift_error_retry", "skilift_error"), message = "x")
  .capture_batches(fail_at = 1L, error = err)
  cnd <- tryCatch(.insert_data_literal(con, '"T"', data.frame(a = 1:5)), error = function(e) e)
  expect_match(conditionMessage(cnd), "duplicated")
})

test_that("a batch that hits a 503 is retried with its requestId, and written once", {
  con <- .write_con()
  .record_sleeps()
  m <- .mock_sequence(list(.json_resp(503), .json_resp(200, list(statementHandle = "h"))))
  httr2::local_mocked_responses(m$mock)
  .insert_data_literal(con, '"T"', data.frame(a = 1:3))
  urls <- m$urls()
  expect_length(urls, 2L)
  expect_equal(.request_id_of(urls[1]), .request_id_of(urls[2]))
  expect_true(grepl("retry=true", urls[2], fixed = TRUE))
})

test_that("pack_by_bytes starts a new batch exactly when the next row would not fit", {
  expect_equal(.pack_by_bytes(c(4, 4, 4, 4), 8), c(1L, 1L, 2L, 2L))
  expect_equal(.pack_by_bytes(c(9, 1), 10), c(1L, 1L))
  expect_equal(.pack_by_bytes(c(6, 6, 6), 10), c(1L, 2L, 3L))
  expect_equal(.pack_by_bytes(integer(), 10), integer())
})

test_that("dbAppendTable and dbCreateTable accept a DBI::Id table name", {
  # Without methods for Id these fell through to DBI's generic versions, which
  # use multi-row parameter binds that skiLift does not support.
  con <- .write_con()
  seen <- new.env()
  testthat::local_mocked_bindings(
    .insert_data = function(conn, table_id, df) { seen$table_id <- as.character(table_id); invisible(NULL) },
    .package = "skiLift"
  )
  dbAppendTable(con, DBI::Id(schema = "MYSCHEMA", table = "MYTABLE"), data.frame(a = 1:2))
  expect_match(seen$table_id, "MYSCHEMA.*MYTABLE")

  sql <- new.env()
  testthat::local_mocked_bindings(
    sf_api_submit = function(con, sql_text, ...) { sql$text <- sql_text; list() },
    .package = "skiLift"
  )
  dbCreateTable(con, DBI::Id(schema = "MYSCHEMA", table = "MYTABLE"), c(a = "INT"))
  expect_match(sql$text, "MYSCHEMA.*MYTABLE")
})
