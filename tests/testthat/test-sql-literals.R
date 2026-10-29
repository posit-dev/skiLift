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
  # 17 significant digits identify any double; whole numbers below 1e15 are digits.
  expect_identical(.format_value(0.5), "0.5")
  expect_identical(.format_value(-2.5), "-2.5")
  expect_identical(.format_value(100000), "100000")
  expect_identical(.format_value(1e15), "1000000000000000")
  expect_identical(.format_value(0.1), "0.10000000000000001")
  expect_identical(.format_value(0.1 + 0.2), "0.30000000000000004")
  expect_identical(.format_value(1/3), "0.33333333333333331")
  expect_identical(.format_value(123456789.123456789), "123456789.12345679")
  expect_identical(.format_value(.Machine$double.xmax), "1.7976931348623157e+308")
  expect_identical(.format_value(3L), "3")
  # and the text parses back to the same double, using the exact parser
  for (x in c(0.1, 0.1 + 0.2, 1/3, pi, 1e-300, 1e300, 5e-324, 123456789.123456789, .Machine$double.xmax, -2.5)) {
    expect_identical(.parse_double(.format_value(x)), x, info = sprintf("%.17g", x))
  }
})

test_that(".parse_double is exact where as.numeric() is not", {
  expect_identical(.parse_double("1.7976931348623157e+308"), .Machine$double.xmax)
  expect_identical(.parse_double("5e-324"), 5e-324)
  expect_identical(.parse_double("0.30000000000000004"), 0.1 + 0.2)
  expect_identical(.parse_double(c("1", NA, "-2.5")), c(1, NA, -2.5))
  # not compared with the R literal 1e300, which R itself parses inexactly
  expect_identical(sprintf("%.17g", .parse_double("1e+300")), "1.0000000000000001e+300")
  expect_identical(.parse_double(c("NaN", "inf", "-inf")), c(NaN, Inf, -Inf))
  expect_identical(.parse_double(c(".5", "+3", "1.")), c(0.5, 3, 1))
  expect_identical(.parse_double(character(0)), numeric(0))
  expect_identical(.parse_double(NA_character_), NA_real_)
})

test_that("DATE reads the whole Snowflake range, and ISO text instead of NA", {
  expect_identical(.convert_date(c("2932896", "-719162", "0", NA)),
                   as.Date(c("9999-12-31", "0001-01-01", "1970-01-01", NA)))
  expect_identical(.convert_date(c("2026-10-21", NA)), as.Date(c("2026-10-21", NA)))
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

# Binary values and field.types ----------------------------------------------------
# A plain list of raw vectors (DBI's standard way to hold binary) used to be
# taken for text, so dbWriteTable() stored the words "as.raw(c(0xde, ...))".

test_that("a list of raw vectors is a BINARY column, and is written as hex", {
  expect_identical(r_to_sf_type(list(as.raw(1:3), NULL, as.raw(255))), "BINARY")
  expect_identical(r_to_sf_type(list("a", "b")), "TEXT")
  expect_identical(r_to_sf_type(list(as.raw(1), "b")), "TEXT")
  expect_identical(r_to_sf_type(list()), "TEXT")
  col <- list(as.raw(c(0xde, 0xad)), NULL, raw(0))
  expect_identical(.format_value(col[1]), "X'DEAD'")
  expect_identical(.format_value(col[2]), "NULL")
  expect_identical(.format_value(col[3]), "X''")
})

test_that("field.types replaces the chosen type, by column name", {
  df <- data.frame(id = 1:2, note = c("a", "b"), x = c(1.5, 2.5))
  expect_identical(.field_types_for(df), c(id = "NUMBER(10, 0)", note = "TEXT", x = "DOUBLE"))
  got <- .field_types_for(df, c(id = "BIGINT", note = "VARCHAR(50)"))
  expect_identical(got, c(id = "BIGINT", note = "VARCHAR(50)", x = "DOUBLE"))
})

test_that("field.types is matched like the column names are, and a typo is an error", {
  df <- data.frame(id = 1:2)
  names(df) <- .maybe_upcase(names(df))
  expect_identical(.field_types_for(df, c(id = "BIGINT"))[["ID"]], "BIGINT")
  expect_error(.field_types_for(df, c(idd = "BIGINT")), "does not have")
  expect_error(.field_types_for(df, c("BIGINT")), "named character vector")
})


test_that("quoting zero strings gives zero strings, not an empty-string literal", {
  con <- new("SnowflakeConnection", account = "a", user = "u", database = "", schema = "",
             warehouse = "", role = "", .auth = list(), .state = .new_conn_state())
  expect_length(dbQuoteString(con, character()), 0L)
  expect_length(dbQuoteLiteral(con, character()), 0L)
  expect_equal(as.character(dbQuoteString(con, c("a", NA))), c("'a'", "NULL"))
})
