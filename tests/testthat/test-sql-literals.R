# SQL literals written by INSERT and dbQuoteString ---------------------------------
# Snowflake treats a backslash inside a single-quoted string as an escape, and
# R's as.character() keeps only 15 significant digits. Both used to corrupt data
# written with dbWriteTable(): 'C:\temp' was stored with a tab in it.

# A small reader for Snowflake's single-quoted string rules, to check that what we
# write reads back as the value we started with.
.read_sf_string <- function(lit) {
  stopifnot(startsWith(lit, "'"), endsWith(lit, "'"))
  chars <- strsplit(substr(lit, 2L, nchar(lit) - 1L), "")[[1]]
  out <- character(); i <- 1L
  while (i <= length(chars)) {
    ch <- chars[i]
    if (ch == "\\") {
      nxt <- chars[i + 1L]
      out <- c(out, switch(nxt, n = "\n", t = "\t", r = "\r", nxt))
      i <- i + 2L
    } else if (ch == "'" && identical(chars[i + 1L], "'")) {
      out <- c(out, "'"); i <- i + 2L
    } else {
      out <- c(out, ch); i <- i + 1L
    }
  }
  paste(out, collapse = "")
}

test_that("strings read back unchanged, including backslashes and quotes", {
  tricky <- c("plain", "O'Brien", "C:\\temp\\new\\table", "back\\slash then n: \\n",
              "\\'", "ends with \\", "tab\there", "line1\nline2", "caf\u00e9 \u2713", "it''s", "\\\\server\\share")
  for (x in tricky) expect_identical(.read_sf_string(.format_value(x)), x, info = x)
})

test_that("dbQuoteString escapes backslashes too", {
  con <- new("SnowflakeConnection", account = "x", user = "u", database = "", schema = "",
             warehouse = "", role = "", .auth = list(), .state = .new_conn_state())
  q <- DBI::dbQuoteString(con, c("a\\b", "it's", NA))
  expect_identical(.read_sf_string(as.character(q[1])), "a\\b")
  expect_identical(.read_sf_string(as.character(q[2])), "it's")
  expect_identical(as.character(q[3]), "NULL")
})

test_that("doubles are written with enough digits to read back exactly", {
  # 15 significant digits when those are exact, otherwise 17, which always is.
  # (R's own as.numeric() cannot be used to check the 17-digit text: it does not
  # parse long decimals exactly. Snowflake's parser does.)
  expect_identical(.format_value(0.1), "0.1")
  expect_identical(.format_value(0.5), "0.5")
  expect_identical(.format_value(-2.5), "-2.5")
  expect_identical(.format_value(1e15), "1e+15")
  expect_identical(.format_value(0.1 + 0.2), "0.30000000000000004")
  expect_identical(.format_value(1/3), "0.33333333333333331")
  expect_identical(.format_value(123456789.123456789), "123456789.12345679")
  expect_identical(.format_value(.Machine$double.xmax), "1.7976931348623157e+308")
  expect_identical(.format_value(3L), "3")
  for (x in c(0.1, 0.5, -2.5, 1e15, 12345.678)) {
    expect_identical(as.numeric(.format_value(x)), x)    # short forms parse exactly in R too
  }
})

test_that("NaN and the infinities are written as casts; NA is NULL", {
  expect_identical(.format_value(NaN), "'NaN'::FLOAT")
  expect_identical(.format_value(Inf), "'inf'::FLOAT")
  expect_identical(.format_value(-Inf), "'-inf'::FLOAT")
  for (na in list(NA, NA_integer_, NA_real_, NA_character_, as.Date(NA), as.POSIXct(NA))) {
    expect_identical(.format_value(na), "NULL")
  }
})

test_that("timestamps are written as UTC wall-clock whatever R's time zone is", {
  x <- as.POSIXct("2026-10-14 12:34:56.123456", tz = "UTC")
  for (tz in c("UTC", "Europe/London", "Pacific/Auckland", "America/Los_Angeles")) {
    withr::local_timezone(tz)
    expect_identical(.format_value(x), "'2026-10-14 12:34:56.123456'", info = tz)
  }
  zoned <- as.POSIXct("2026-10-14 12:34:56", tz = "Asia/Kolkata")     # +05:30
  expect_identical(.format_value(zoned), "'2026-10-14 07:04:56.000000'")
})

test_that("dates, times, logicals and integer64 have proper literals", {
  expect_identical(.format_value(as.Date("2026-10-14")), "'2026-10-14'")
  expect_identical(.format_value(TRUE), "TRUE")
  expect_identical(.format_value(as.difftime(45296.5, units = "secs")), "'12:34:56.500000'")
  expect_identical(.format_value(as.difftime(2, units = "hours")), "'02:00:00.000000'")
  skip_if_not_installed("bit64")
  expect_identical(.format_value(bit64::as.integer64("9007199254740993")), "9007199254740993")
})

test_that("binary values are written as hex literals", {
  skip_if_not_installed("blob")
  expect_identical(.format_value(blob::blob(as.raw(c(0xde, 0xad, 0xbe, 0xef)))), "X'DEADBEEF'")
  expect_identical(.format_value(blob::blob(NULL)), "NULL")
})
