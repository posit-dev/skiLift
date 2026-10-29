# Reliability against a live Snowflake account -----------------------------------
#
# Long statements, wide-row writes and partial-write reporting, checked against
# what Snowflake actually holds. Opt-in like test-fidelity-live.R: set
# SKILIFT_FIDELITY=true and SKILIFT_PROFILE. Takes about two minutes, most of it
# two statements that wait 50 seconds on purpose. It creates and drops its own
# scratch database.

skip_on_cran()
skip_if(
  !nzchar(Sys.getenv("SKILIFT_FIDELITY", "")),
  "Live tests skipped (set SKILIFT_FIDELITY=true to enable)"
)

con <- suppressMessages(DBI::dbConnect(
  Snowflake(), name = Sys.getenv("SKILIFT_PROFILE", "default")
))
DB <- paste0("SKILIFT_RELIABILITY_", toupper(paste(sample(c(letters, 0:9), 8), collapse = "")))
DBI::dbExecute(con, paste("CREATE DATABASE", DB))
withr::defer(try(DBI::dbExecute(con, paste("DROP DATABASE IF EXISTS", DB)), silent = TRUE), teardown_env())
fq <- function(name) paste0(DB, ".PUBLIC.", name)
sf <- function(sql) { r <- DBI::dbGetQuery(con, sql); names(r) <- tolower(names(r)); r }

test_that("a query that outlasts Snowflake's 45 s window returns its rows", {
  # Before 29 Oct 2026 this returned a data frame with no rows and no columns.
  res <- DBI::dbGetQuery(con, "SELECT SYSTEM$WAIT(50) AS w, 1 AS x")
  expect_equal(dim(res), c(1L, 2L))
  expect_equal(res$X, 1L)
})

test_that("a long statement returns only when it has finished", {
  # Before, dbExecute() came back after 45 s with the statement still running.
  started <- Sys.time()
  DBI::dbExecute(con, "CALL SYSTEM$WAIT(50)")
  expect_gte(as.numeric(Sys.time() - started, units = "secs"), 49)
})

test_that("a statement past the deadline raises a timeout that names it", {
  withr::local_options(skiLift.timeout = 20L)
  cnd <- tryCatch(DBI::dbGetQuery(con, "SELECT SYSTEM$WAIT(120) AS w"), error = function(e) e)
  expect_s3_class(cnd, "skilift_error_timeout")
  expect_true(nzchar(cnd$statement_handle %||% "") || !is.null(cnd$request_id))
})

test_that("rows too wide for one request are split by bytes and all arrive", {
  # 400 rows x ~10 KB is about 4 MB: far over the 1 MB a request should hold.
  n <- 400L
  df <- data.frame(id = seq_len(n), txt = strrep("abcdefghij", 1000), stringsAsFactors = FALSE)
  DBI::dbWriteTable(con, DBI::Id(DB, "PUBLIC", "WIDE"), df)
  got <- sf(sprintf("SELECT COUNT(*) AS n, COUNT(DISTINCT id) AS d, MIN(LENGTH(txt)) AS lo, MAX(LENGTH(txt)) AS hi FROM %s", fq("WIDE")))
  expect_equal(as.numeric(got$n), n)
  expect_equal(as.numeric(got$d), n)
  expect_equal(as.numeric(c(got$lo, got$hi)), c(10000, 10000))
})

test_that("a row larger than the byte budget is refused before anything is sent", {
  withr::local_options(skiLift.insert_batch_bytes = 5000)
  df <- data.frame(id = 1:3, txt = c("a", strrep("y", 6000), "c"), stringsAsFactors = FALSE)
  cnd <- tryCatch(DBI::dbWriteTable(con, DBI::Id(DB, "PUBLIC", "BIGROW"), df), error = function(e) e)
  expect_s3_class(cnd, "skilift_error_write")
  expect_equal(cnd$outcome, "not_executed")
  expect_equal(cnd$failed_batch$first_row, 2L)
})

test_that("a failure part-way reports exactly the rows that are in the table", {
  DBI::dbExecute(con, sprintf("CREATE OR REPLACE TABLE %s (n NUMBER(3,0))", fq("NARROW")))
  # 25 rows, 10 per batch; row 23 does not fit NUMBER(3,0), so batch 3 fails
  # and batches 1 and 2 (20 rows) are already committed.
  withr::local_options(skiLift.insert_batch_size = 10L)
  df <- data.frame(n = c(1:22, 99999L, 24L, 25L))
  cnd <- tryCatch(DBI::dbAppendTable(con, DBI::Id(DB, "PUBLIC", "NARROW"), df), error = function(e) e)
  expect_s3_class(cnd, "skilift_error_write")
  expect_equal(cnd$outcome, "failed")
  expect_equal(cnd$rows_committed, 20L)
  expect_equal(cnd$batches_committed, 2L)
  expect_equal(cnd$failed_batch$first_row, 21L)
  expect_s3_class(cnd$parent, "skilift_error_sql")
  in_table <- sf(sprintf("SELECT COUNT(*) AS n FROM %s", fq("NARROW")))$n
  expect_equal(as.numeric(in_table), cnd$rows_committed)
})

test_that("transactions refuse rather than pretend", {
  expect_error(DBI::dbBegin(con), class = "skilift_error_config")
})

test_that("a Snowflake Scripting block is atomic through skiLift", {
  DBI::dbExecute(con, sprintf("CREATE OR REPLACE TABLE %s (x INT)", fq("ATOM")))
  block <- function(tail) sprintf(
    "EXECUTE IMMEDIATE $$ BEGIN BEGIN TRANSACTION; INSERT INTO %s VALUES (1); %s END $$",
    fq("ATOM"), tail
  )
  DBI::dbExecute(con, block("ROLLBACK;"))
  expect_equal(as.numeric(sf(sprintf("SELECT COUNT(*) AS n FROM %s", fq("ATOM")))$n), 0)
  DBI::dbExecute(con, block("COMMIT;"))
  expect_equal(as.numeric(sf(sprintf("SELECT COUNT(*) AS n FROM %s", fq("ATOM")))$n), 1)
})
