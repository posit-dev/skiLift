# Fetching a result in pieces: dbFetch(res, n = k) repeatedly
# =============================================================================
# Until 21 Oct 2026, dbFetch(res, n = k) with k smaller than the first
# partition returned k rows and discarded the rest of that partition, and every
# later call returned nothing. `while (!dbHasCompleted(res)) dbFetch(res, n =
# 1000)` -- the standard way to process a large result in chunks -- silently
# lost all rows after the first 1000. The mocks below stand in for Snowflake's
# partitions so each shape of result can be tested without an account.

.mock_result <- function(partition_sizes, rows_affected = -1L) {
  starts <- c(0L, cumsum(partition_sizes))[seq_along(partition_sizes)]
  parts <- Map(function(start, size) data.frame(n = start + seq_len(size)),
               starts, partition_sizes)
  meta <- list(num_partitions = length(parts), statement_handle = "h",
               num_rows = sum(partition_sizes))
  con <- new("SnowflakeConnection", account = "a", user = "u", database = "",
             schema = "", warehouse = "", role = "", .auth = list(),
             .state = .new_conn_state())
  res <- new("SnowflakeResult", connection = con, statement = "select",
             .resp_body = list(), .meta = meta,
             .state = .new_result_state(rows_affected))
  list(res = res, parts = parts)
}

.with_partitions <- function(m, code) {
  testthat::local_mocked_bindings(
    sf_parse_response = function(resp_body, conn = NULL, inherit = NULL) {
      list(data = m$parts[[1L]])
    },
    .fetch_partitions_sequential = function(con, handle, partition_indices, meta) {
      lapply(partition_indices, function(i) m$parts[[i + 1L]])
    },
    sf_fetch_partitions_parallel = function(con, handle, partition_indices, meta) {
      lapply(partition_indices, function(i) m$parts[[i + 1L]])
    },
    .empty_df_from_meta = function(meta) data.frame(n = integer()),
    .package = "skiLift"
  )
  force(code)
}

.fetch_in_chunks <- function(res, n) {
  out <- list()
  guard <- 0L
  while (!DBI::dbHasCompleted(res) && (guard <- guard + 1L) < 100L) {
    out[[length(out) + 1L]] <- DBI::dbFetch(res, n = n)
  }
  out
}

test_that("fetching 3 at a time from one partition returns every row, in order", {
  m <- .mock_result(10L)
  chunks <- .with_partitions(m, .fetch_in_chunks(m$res, 3L))
  expect_equal(vapply(chunks, nrow, integer(1)), c(3L, 3L, 3L, 1L))
  expect_equal(unlist(lapply(chunks, `[[`, "n")), 1:10)
  expect_equal(DBI::dbGetRowCount(m$res), 10L)
})

test_that("chunks that span several partitions lose and repeat nothing", {
  m <- .mock_result(c(4L, 4L, 2L))
  chunks <- .with_partitions(m, .fetch_in_chunks(m$res, 3L))
  expect_equal(unlist(lapply(chunks, `[[`, "n")), 1:10)
  expect_true(all(vapply(chunks, nrow, integer(1)) <= 3L))
  expect_equal(DBI::dbGetRowCount(m$res), 10L)
})

test_that("a chunk larger than a partition reads on into the next one", {
  m <- .mock_result(c(2L, 2L, 2L, 2L))
  chunks <- .with_partitions(m, .fetch_in_chunks(m$res, 5L))
  expect_equal(unlist(lapply(chunks, `[[`, "n")), 1:8)
  expect_equal(vapply(chunks, nrow, integer(1)), c(5L, 3L))
})

test_that("fetch(n) then fetch(-1) returns exactly the rest", {
  m <- .mock_result(c(4L, 4L, 2L))
  got <- .with_partitions(m, list(
    a = DBI::dbFetch(m$res, n = 5L),
    b = DBI::dbFetch(m$res, n = -1)
  ))
  expect_equal(got$a$n, 1:5)
  expect_equal(got$b$n, 6:10)
  expect_true(DBI::dbHasCompleted(m$res))
  expect_equal(nrow(.with_partitions(m, DBI::dbFetch(m$res, n = 5L))), 0L)
})

test_that("n = Inf is the same as n = -1, and n = 0 takes nothing", {
  m <- .mock_result(c(3L, 3L))
  got <- .with_partitions(m, list(
    zero = DBI::dbFetch(m$res, n = 0L),
    all  = DBI::dbFetch(m$res, n = Inf)
  ))
  expect_equal(nrow(got$zero), 0L)
  expect_equal(got$all$n, 1:6)
})

test_that("a zero-row result completes and returns an empty frame", {
  m <- .mock_result(0L)
  m$parts <- list(data.frame(n = integer()))
  got <- .with_partitions(m, DBI::dbFetch(m$res, n = 3L))
  expect_equal(nrow(got), 0L)
  expect_true(DBI::dbHasCompleted(m$res))
})

test_that("dbHasCompleted is FALSE until the last buffered row has been returned", {
  m <- .mock_result(c(4L, 4L))
  .with_partitions(m, {
    DBI::dbFetch(m$res, n = 2L)
    expect_false(DBI::dbHasCompleted(m$res))
    DBI::dbFetch(m$res, n = 5L)
    expect_false(DBI::dbHasCompleted(m$res))
    DBI::dbFetch(m$res, n = 5L)
    expect_true(DBI::dbHasCompleted(m$res))
  })
})

test_that("a statement is complete as soon as it has run; a query affects 0 rows", {
  stmt <- .mock_result(1L, rows_affected = 3L)
  expect_true(DBI::dbHasCompleted(stmt$res))
  expect_equal(DBI::dbGetRowsAffected(stmt$res), 3L)
  qry <- .mock_result(1L)
  expect_equal(DBI::dbGetRowsAffected(qry$res), 0L)
  expect_false(DBI::dbHasCompleted(qry$res))
})
