# Whole numbers ------------------------------------------------------------------
# A column's R type is decided by its Snowflake precision and scale, never by
# the values in the result. Snowflake stores INTEGER, BIGINT, NUMBER(38,0) etc.
# identically, so precision is all there is to go on.

test_that("up to 9 digits is always integer", {
  out <- sf_convert_column(c("1", "2", NA, "42"), "fixed", 0L, 9L)
  expect_type(out, "integer")
  expect_equal(out, c(1L, 2L, NA, 42L))
})

test_that("larger whole numbers are numeric by default, whatever the values", {
  # The same NUMBER(38,0) column used to be integer or double depending on
  # whether any value in the result happened to exceed 2^31.
  small <- sf_convert_column(c("7", NA), "fixed", 0L, 38L)
  big   <- sf_convert_column(c("7", "2147483648", "9007199254740992"), "fixed", 0L, 38L)
  expect_type(small, "double")
  expect_type(big, "double")
  expect_equal(big, c(7, 2147483648, 9007199254740992))
  expect_type(sf_convert_column("1", "fixed", 0L, 10L), "double")
})

test_that("unknown precision is treated as large, not as integer", {
  expect_type(sf_convert_column("1", "fixed", 0L), "double")
  expect_type(sf_convert_column("1", "fixed", 0L, 0L), "double")
})

test_that("bigint chooses the type for large whole numbers", {
  x <- c("7", "2147483648")
  expect_identical(sf_convert_column(x, "fixed", 0L, 38L, "character"), x)
  expect_type(sf_convert_column("7", "fixed", 0L, 38L, "integer"), "integer")
  expect_type(sf_convert_column(x, "fixed", 0L, 38L, "numeric"), "double")
})

test_that('bigint = "integer64" keeps every 64-bit value exactly', {
  skip_if_not_installed("bit64")
  big <- sf_convert_column(c("7", "2147483648", "9007199254740993"), "fixed", 0L, 38L, "integer64")
  expect_s3_class(big, "integer64")
  expect_equal(as.character(big), c("7", "2147483648", "9007199254740993"))
})

test_that("a value that does not fit is NA with a warning, never silent", {
  expect_warning(
    out <- sf_convert_column(c("7", "2147483648"), "fixed", 0L, 38L, "integer"),
    "did not fit integer"
  )
  expect_equal(out, c(7L, NA))
  skip_if_not_installed("bit64")
  expect_warning(
    out <- sf_convert_column("99999999999999999999999999999999999999", "fixed", 0L, 38L, "integer64"),
    "did not fit integer64"
  )
  expect_true(is.na(out))
})

test_that("a scale above zero is always double", {
  out <- sf_convert_column("1.50", "fixed", 2L, 10L)
  expect_type(out, "double")
  expect_equal(out, 1.5)
})

test_that("dbConnect() rejects an unknown bigint before connecting", {
  expect_error(dbConnect(Snowflake(), account = "x", bigint = "nope"), "bigint")
})

test_that("the skiLift.bigint option is the default for bigint", {
  withr::local_options(skiLift.bigint = "nope")
  expect_error(dbConnect(Snowflake(), account = "x"), "bigint")
})

test_that("zero-row results have the same column classes as non-empty ones", {
  types <- data.frame(
    name = c("int9", "big", "dec", "dbl", "txt", "lgl", "dt", "tm", "ntz", "ltz", "tz", "var"),
    type = c("fixed", "fixed", "fixed", "real", "text", "boolean", "date", "time",
             "timestamp_ntz", "timestamp_ltz", "timestamp_tz", "variant"),
    scale     = c(0L, 0L, 2L, 0L, 0L, 0L, 0L, 9L, 9L, 9L, 9L, 0L),
    precision = c(9L, 38L, 10L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L),
    nullable = TRUE, stringsAsFactors = FALSE
  )
  meta <- list(columns = types, bigint = "numeric")
  row <- list(list("7", "7", "1.50", "3.5", "a", "true", "20740", "45296.5",
                   "1791961496.000000000", "1791961496.000000000",
                   "1791961496.000000000 1770", "{}"))
  full  <- .json_data_to_df(row, meta)
  empty <- .empty_df_from_meta(meta)
  expect_equal(nrow(empty), 0L)
  expect_equal(lapply(empty, class), lapply(full, class))
})

test_that("sf_convert_column handles decimal fixed", {
  x <- c("1.5", "2.7", NA)
  result <- sf_convert_column(x, "fixed", 2L)
  expect_type(result, "double")
  expect_equal(result, c(1.5, 2.7, NA))
})

test_that("sf_convert_column handles booleans", {
  x <- c("true", "false", NA, "1", "0")
  result <- sf_convert_column(x, "boolean")
  expect_equal(result, c(TRUE, FALSE, NA, TRUE, FALSE))
})

test_that("sf_convert_column handles text passthrough", {
  x <- c("hello", "world", NA)
  result <- sf_convert_column(x, "text")
  expect_equal(result, c("hello", "world", NA))
})

test_that("sf_convert_column handles dates as epoch days", {
  x <- c("0", "19723", NA)
  result <- sf_convert_column(x, "date")
  expect_s3_class(result, "Date")
  expect_equal(result[1], as.Date("1970-01-01"))
  expect_true(is.na(result[3]))
})

test_that("sf_convert_column handles real/float", {
  x <- c("3.14", "2.72", NA)
  result <- sf_convert_column(x, "real")
  expect_type(result, "double")
  expect_equal(result, c(3.14, 2.72, NA))
})

test_that("r_to_sf_type maps R types to Snowflake types", {
  expect_equal(r_to_sf_type(1L), "NUMBER(10, 0)")
  expect_equal(r_to_sf_type(1.0), "DOUBLE")
  expect_equal(r_to_sf_type("a"), "TEXT")
  expect_equal(r_to_sf_type(TRUE), "BOOLEAN")
  expect_equal(r_to_sf_type(Sys.Date()), "DATE")
  expect_equal(r_to_sf_type(Sys.time()), "TIMESTAMP_NTZ")
  expect_equal(r_to_sf_type(as.raw(0)), "BINARY")
})

test_that("integer overflow falls back to double", {
  big <- as.character(.Machine$integer.max + 1)
  result <- sf_convert_column(c(big), "fixed", 0L)
  expect_type(result, "double")
})


# TIME and TIMESTAMP_TZ ---------------------------------------------------------
# The payloads below are what the SQL API actually returned for
#   SELECT '12:34:56.5'::TIME, '2026-10-14 12:34:56 +0530'::TIMESTAMP_TZ
# (both scale 9). An earlier version assumed TIME arrived as nanoseconds and
# that TIMESTAMP_TZ was a bare number, so it returned values a billion times
# too small and NA respectively, and nothing caught it because no test used a
# real payload.

test_that("TIME is read as seconds since midnight", {
  out <- sf_convert_column(c("45296.500000000", "0.000000000", NA, "86399.999999999"), "time", 9L)
  expect_equal(as.numeric(out), c(45296.5, 0, NA, 86399.999999999))
  expect_equal(format(out[1]), "12:34:56.5")
})

test_that("TIMESTAMP_TZ returns the instant, not NA", {
  out <- sf_convert_column(
    c("1791961496.000000000 1770", "1792010096.123456789 1080", NA),
    "timestamp_tz", 9L
  )
  expect_s3_class(out, "POSIXct")
  expect_false(is.na(out[1]))
  # 12:34:56 +05:30 is 07:04:56 UTC; 12:34:56.123456 -08:00 is 20:34:56.123456 UTC
  expect_equal(format(out[1], "%Y-%m-%d %H:%M:%S", tz = "UTC"), "2026-10-14 07:04:56")
  expect_equal(format(out[2], "%Y-%m-%d %H:%M:%OS6", tz = "UTC"), "2026-10-14 20:34:56.123456")
  expect_true(is.na(out[3]))
})

test_that("TIMESTAMP_TZ also copes with a bare epoch value", {
  out <- sf_convert_column("1791961496.000000000", "timestamp_tz", 9L)
  expect_equal(format(out, "%H:%M:%S", tz = "UTC"), "07:04:56")
})

test_that("TIME and TIMESTAMP_TZ survive the full parse path", {
  meta <- list(columns = data.frame(
    name = c("T", "Z"), type = c("time", "timestamp_tz"),
    nullable = TRUE, scale = 9L, precision = 0L, stringsAsFactors = FALSE
  ))
  raw <- list(list("45296.500000000", "1791961496.000000000 1770"))
  df <- .json_data_to_df(raw, meta)
  expect_equal(as.numeric(df$T), 45296.5)
  expect_false(is.na(df$Z))
})


# timezone_out -------------------------------------------------------------------
# TIMESTAMP_LTZ and TIMESTAMP_TZ are moments in time; timezone_out only chooses
# the zone they are labelled with. The default is the Snowflake session's zone,
# which is what Snowflake's SQL functions and its Python and ADBC drivers use.

.conn_with_state <- function(...) {
  state <- new.env(parent = emptyenv())
  vals <- list(...)
  for (n in names(vals)) assign(n, vals[[n]], envir = state)
  new("SnowflakeConnection", account = "x", user = "u", database = "", schema = "",
      warehouse = "", role = "", .auth = list(), .state = state)
}

test_that("LTZ and TZ columns take the requested zone; the moment is unchanged", {
  ltz <- sf_convert_column("1792006496.000000000", "timestamp_ltz", 9L, tz_out = "America/Los_Angeles")
  utc <- sf_convert_column("1792006496.000000000", "timestamp_ltz", 9L, tz_out = "UTC")
  expect_equal(attr(ltz, "tzone"), "America/Los_Angeles")
  expect_equal(attr(utc, "tzone"), "UTC")
  expect_equal(as.numeric(ltz), as.numeric(utc))
  expect_equal(format(ltz, "%H:%M"), "12:34")        # 19:34 UTC is 12:34 in Los Angeles
  tz <- sf_convert_column("1791961496.000000000 1770", "timestamp_tz", 9L, tz_out = "Europe/London")
  expect_equal(attr(tz, "tzone"), "Europe/London")
  expect_equal(format(tz, "%H:%M:%S", tz = "UTC"), "07:04:56")
})

test_that("NTZ stays labelled UTC whatever timezone_out is", {
  ntz <- sf_convert_column("1792006496.000000000", "timestamp_ntz", 9L, tz_out = "Asia/Tokyo")
  expect_equal(attr(ntz, "tzone"), "UTC")
})

test_that("zero-row LTZ columns carry the zone too", {
  meta <- list(columns = data.frame(name = "L", type = "timestamp_ltz", nullable = TRUE,
                                    scale = 9L, precision = 0L, stringsAsFactors = FALSE),
               tz_out = "America/Los_Angeles")
  expect_equal(attr(.empty_df_from_meta(meta)$L, "tzone"), "America/Los_Angeles")
})

test_that("timezone_out = 'local' is R's zone, a name is used as given", {
  expect_equal(.resolve_tz_out(.conn_with_state(timezone_out = "Asia/Tokyo")), "Asia/Tokyo")
  withr::local_timezone("Pacific/Auckland")
  expect_equal(.resolve_tz_out(.conn_with_state(timezone_out = "local")), "Pacific/Auckland")
})

test_that("timezone_out = 'database' uses the remembered session zone without a query", {
  con <- .conn_with_state(timezone_out = "database", session_tz = "America/Los_Angeles")
  expect_equal(.resolve_tz_out(con), "America/Los_Angeles")
})

test_that("an unknown timezone_out is rejected before connecting", {
  expect_error(dbConnect(Snowflake(), account = "x", timezone_out = "Mars/Olympus"), "timezone_out")
  withr::local_options(skiLift.timezone_out = "Mars/Olympus")
  expect_error(dbConnect(Snowflake(), account = "x"), "timezone_out")
  expect_equal(.check_timezone_out("UTC"), "UTC")
  expect_equal(.check_timezone_out("database"), "database")
})
