test_that("sf_type_to_r maps fixed types correctly", {
  expect_equal(sf_type_to_r("fixed", 0L), "integer")
  expect_equal(sf_type_to_r("fixed", 2L), "double")
})

test_that("sf_type_to_r maps common types", {
  expect_equal(sf_type_to_r("text"), "character")
  expect_equal(sf_type_to_r("real"), "double")
  expect_equal(sf_type_to_r("float"), "double")
  expect_equal(sf_type_to_r("boolean"), "logical")
  expect_equal(sf_type_to_r("date"), "double")
  expect_equal(sf_type_to_r("timestamp_ntz"), "double")
  expect_equal(sf_type_to_r("timestamp_ltz"), "double")
  expect_equal(sf_type_to_r("timestamp_tz"), "double")
  expect_equal(sf_type_to_r("time"), "double")
  expect_equal(sf_type_to_r("variant"), "character")
  expect_equal(sf_type_to_r("array"), "character")
  expect_equal(sf_type_to_r("object"), "character")
})

test_that("sf_type_to_r returns character for unknown types", {
  expect_equal(sf_type_to_r("somethingelse"), "character")
})

test_that("sf_convert_column handles integer fixed", {
  x <- c("1", "2", NA, "42")
  result <- sf_convert_column(x, "fixed", 0L)
  expect_type(result, "integer")
  expect_equal(result, c(1L, 2L, NA, 42L))
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
