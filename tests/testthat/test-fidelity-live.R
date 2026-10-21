# Data fidelity against a live Snowflake account ---------------------------------
#
# Three things can corrupt data, and each needs its own test:
#
#   INGRESS     R -> Snowflake. Checked INSIDE Snowflake (hash, hex bytes, equality
#               against a literal), never by reading the data back through R:
#               a write bug and a read bug can cancel each other out.
#   EGRESS      Snowflake -> R. The values are built inside Snowflake from SQL
#               expressions that do not go through skiLift's write path.
#   ROUND TRIP  write, then read, and compare. Catches what the first two miss.
#
# Every bug found so far (TIME, TIMESTAMP_TZ, backslashes in strings, 15-digit
# doubles, local-time timestamps) would have been caught by one of these.
#
# Skipped unless SKILIFT_FIDELITY=true. It creates and drops a scratch database,
# so it needs CREATE DATABASE. Set SKILIFT_PROFILE to a connections.toml profile
# (default: "default").
#
# Adding a type or changing a converter? Add the awkward values to the matrices
# below first. See internal/testing-data-fidelity.md in the project repository
# (or CONTRIBUTING for the short version).

skip_on_cran()
skip_if(
  !nzchar(Sys.getenv("SKILIFT_FIDELITY", "")),
  "Live fidelity tests skipped (set SKILIFT_FIDELITY=true to enable)"
)

con <- suppressMessages(DBI::dbConnect(
  Snowflake(), name = Sys.getenv("SKILIFT_PROFILE", "default"), bigint = "character"
))
DB <- paste0("SKILIFT_FIDELITY_", toupper(paste(sample(c(letters, 0:9), 8), collapse = "")))
DBI::dbExecute(con, paste("CREATE DATABASE", DB))
withr::defer(try(DBI::dbExecute(con, paste("DROP DATABASE IF EXISTS", DB)), silent = TRUE), teardown_env())

tbl_id <- function(name) DBI::Id(DB, "PUBLIC", name)
fq <- function(name) paste0(DB, ".PUBLIC.", name)
sf <- function(sql) { r <- DBI::dbGetQuery(con, sql); names(r) <- tolower(names(r)); r }

hex_of <- function(x) toupper(vapply(x, function(s) {
  if (is.na(s)) NA_character_ else paste(sprintf("%02x", as.integer(charToRaw(enc2utf8(s)))), collapse = "")
}, "", USE.NAMES = FALSE))
from_hex <- function(h) vapply(h, function(s) {
  if (is.na(s)) NA_character_ else if (!nzchar(s)) "" else
    rawToChar(as.raw(strtoi(substring(s, seq(1, nchar(s), 2), seq(2, nchar(s), 2)), 16L)))
}, "", USE.NAMES = FALSE)
# YYYY-MM-DD without relying on strftime, which prints year 1 differently by platform
fmt_date <- function(d) { lt <- as.POSIXlt(d, tz = "UTC"); sprintf("%04d-%02d-%02d", lt$year + 1900L, lt$mon + 1L, lt$mday) }
# distance in units in the last place; R's own parser is allowed this much
ulps <- function(a, b) ifelse(a == b, 0, abs(a - b) / (.Machine$double.eps * pmax(abs(a), abs(b))))

# ---- the awkward values ----------------------------------------------------------

strings <- c(
  "", " ", "  leading and trailing  ", "NULL", "NA", "null", "\\", "\\\\", "\\n", "\\t", "\\'", "ends with \\",
  "'", "''", "it's", "\"", "%", "_", "--", "/*", ";", "$$", "01234", "1e5", "true", "\\N",
  "C:\\temp\\new\\table", "line1\nline2", "crlf\r\nend", "tab\there", "\u00e9", "e\u0301",       # e + combining accent
  "\u4e2d\u6587", "\u0645\u0631\u062d\u0628\u0627", "\U0001F600", "a\U0001F600b", "\u2713 \\n",
  strrep("x", 10000), strrep("\\'", 2000)
)
doubles <- c(0, -0.0, 1, -1, 0.1, 0.1 + 0.2, 1/3, pi, 1e-300, 1e300, 5e-324, .Machine$double.eps,
             .Machine$double.xmax, 2^53, 2^53 + 2, 123456789.123456789, 1e15, 1e16, -2.5, 100000, 1e5 + 0.1)
dates <- as.Date(c("1970-01-01", "1969-12-31", "1900-01-01", "0001-01-01", "9999-12-31", "2000-02-29",
                   "2026-10-21", "1582-10-04"))
stamps <- as.POSIXct(c("1970-01-01 00:00:00", "1969-07-20 20:17:40.5", "2026-03-29 01:30:00",
                       "2026-10-25 01:30:00.123456", "2000-01-01 00:00:00", "2026-12-31 23:59:59.999999",
                       "1900-01-01 00:00:00"), tz = "UTC")
times <- as.difftime(c(0, 1, 45296.5, 86399, 86399.999999, 3600), units = "secs")
ints <- c(0L, 1L, -1L, .Machine$integer.max, -.Machine$integer.max)

# ---- INGRESS: R -> Snowflake, checked inside Snowflake -----------------------------

test_that("ingress: strings reach Snowflake byte for byte", {
  df <- data.frame(id = seq_along(strings), txt = c(strings[-1], NA)[seq_along(strings)], stringsAsFactors = FALSE)
  df$txt[1] <- strings[1]; df$txt[nrow(df)] <- NA
  DBI::dbWriteTable(con, tbl_id("T_STR"), df)
  got <- sf(sprintf("SELECT id, txt IS NULL AS isnull, LENGTH(txt) AS len, HEX_ENCODE(TO_BINARY(txt, 'UTF-8')) AS hex FROM %s ORDER BY id", fq("T_STR")))
  for (i in seq_len(nrow(df))) {
    want <- df$txt[i]
    info <- paste0("row ", i, ": ", substr(encodeString(want %||% "NA"), 1, 40))
    if (is.na(want)) { expect_true(got$isnull[i], info = info); next }
    expect_identical(toupper(got$hex[i]), hex_of(want), info = info)
    expect_equal(as.numeric(got$len[i]), nchar(want, "chars"), info = info)
  }
})

test_that("ingress: doubles are stored exactly, including NaN and the infinities", {
  x <- c(doubles, NaN, Inf, -Inf, NA)
  DBI::dbWriteTable(con, tbl_id("T_DBL"), data.frame(id = seq_along(x), x = x))
  checks <- vapply(seq_along(x), function(i) {
    cond <- if (is.na(x[i]) && !is.nan(x[i])) "x IS NULL"
            else if (is.nan(x[i])) "x = 'NaN'::FLOAT"
            else if (is.infinite(x[i])) sprintf("x = '%s'::FLOAT", if (x[i] > 0) "inf" else "-inf")
            else sprintf("x = '%s'::FLOAT", sprintf("%.17g", x[i]))     # Snowflake's parser is exact
    sf(sprintf("SELECT COALESCE(%s, FALSE) AS ok FROM %s WHERE id = %d", cond, fq("T_DBL"), i))$ok
  }, logical(1))
  for (i in which(!checks)) fail(sprintf("double %s was not stored exactly", sprintf("%.17g", x[i])))
  expect_true(all(checks))
})

test_that("ingress: integers, logicals, dates, times, timestamps and binary are stored as sent", {
  for (tz in c("UTC", "Pacific/Auckland", "America/New_York")) {
    withr::local_timezone(tz)
    nm <- paste0("T_MIX_", gsub("[^A-Z]", "", toupper(tz)))
    n <- max(length(ints), length(dates), length(stamps), length(times))
    rep_to <- function(x) x[(seq_len(n) - 1L) %% length(x) + 1L]
    df <- data.frame(id = seq_len(n), i = rep_to(ints), l = rep_to(c(TRUE, FALSE, NA)), d = rep_to(dates), tm = rep_to(times))
    df$ts <- rep_to(stamps)
    bytes <- lapply(seq_len(n), function(k) if (k %% 4 == 0) NULL else as.raw(c(k, 0, 255, 16)))
    df$bin <- blob::new_blob(bytes)      # a blob ...
    df$binlist <- bytes                  # ... and a plain list of raw vectors, which DBI also allows
    DBI::dbWriteTable(con, tbl_id(nm), df)
    g <- sf(sprintf(paste("SELECT id, i::VARCHAR AS i, l::VARCHAR AS l, TO_VARCHAR(d, 'YYYY-MM-DD') AS d,",
                          "TO_VARCHAR(tm, 'HH24:MI:SS.FF6') AS tm, TO_VARCHAR(ts, 'YYYY-MM-DD HH24:MI:SS.FF6') AS ts,",
                          "HEX_ENCODE(bin) AS bin, HEX_ENCODE(binlist) AS binlist FROM %s ORDER BY id"), fq(nm)))
    secs <- as.numeric(df$tm, units = "secs")
    want_tm <- sprintf("%02d:%02d:%09.6f", as.integer(secs %/% 3600), as.integer((secs %% 3600) %/% 60), secs %% 60)
    expect_identical(as.character(g$i), as.character(df$i), info = tz)
    expect_identical(as.character(g$l), ifelse(is.na(df$l), NA_character_, ifelse(df$l, "true", "false")), info = tz)
    expect_identical(g$d, fmt_date(df$d), info = tz)
    expect_identical(g$tm, want_tm, info = tz)
    expect_identical(g$ts, format(df$ts, "%Y-%m-%d %H:%M:%OS6", tz = "UTC"), info = paste("timestamps with R in", tz))
    want_bin <- vapply(df$bin, function(r) if (is.null(r)) NA_character_ else paste(sprintf("%02X", as.integer(r)), collapse = ""), "")
    expect_identical(toupper(g$bin), want_bin, info = paste("blob column,", tz))
    expect_identical(toupper(g$binlist), want_bin, info = paste("plain list-of-raw column,", tz))
  }
})

# ---- EGRESS: Snowflake -> R, values built inside Snowflake -------------------------

test_that("egress: strings built from bytes in Snowflake arrive unchanged", {
  hex <- hex_of(strings)
  sql <- paste(sprintf("SELECT %d AS id, HEX_DECODE_STRING('%s') AS txt", seq_along(strings), hex), collapse = " UNION ALL ")
  got <- sf(paste("SELECT * FROM (", sql, ") ORDER BY id"))
  for (i in seq_along(strings)) {
    expect_identical(got$txt[i], strings[i], info = paste("row", i, substr(encodeString(strings[i]), 1, 40)))
  }
})

test_that("egress: doubles, whole numbers, dates, times and timestamps arrive as built", {
  for (x in doubles) {
    got <- sf(sprintf("SELECT '%s'::FLOAT AS v", sprintf("%.17g", x)))$v
    # R's own as.numeric() reads 17-digit text up to a few units in the last place off
    expect_lte(ulps(got, x), 4, label = sprintf("units in the last place for %.17g", x))
  }
  big <- sf("SELECT 9007199254740993::NUMBER(38,0) AS v, -9223372036854775807::NUMBER(38,0) AS w, 99999999999999999999999999999999999999::NUMBER(38,0) AS z")
  expect_identical(c(big$v, big$w, big$z), c("9007199254740993", "-9223372036854775807", "99999999999999999999999999999999999999"))
  for (d in fmt_date(dates)) {
    expect_identical(fmt_date(sf(sprintf("SELECT '%s'::DATE AS v", d))$v), d, info = d)
  }
  for (k in seq_along(times)) {
    s <- as.numeric(times[k], units = "secs")
    lit <- sprintf("%02d:%02d:%09.6f", as.integer(s %/% 3600), as.integer((s %% 3600) %/% 60), s %% 60)
    expect_equal(as.numeric(sf(sprintf("SELECT '%s'::TIME AS v", lit))$v), s, tolerance = 1e-9, info = lit)
  }
  for (k in seq_along(stamps)) {
    lit <- format(stamps[k], "%Y-%m-%d %H:%M:%OS6", tz = "UTC")
    got <- sf(sprintf("SELECT '%s'::TIMESTAMP_NTZ AS v", lit))$v
    expect_equal(as.numeric(got), as.numeric(stamps[k]), tolerance = 1e-6, info = lit)
    expect_equal(attr(got, "tzone"), "UTC")
  }
})

test_that("egress: TIMESTAMP_LTZ and TIMESTAMP_TZ are the right moment in every zone", {
  for (tz in c("UTC", "Pacific/Auckland", "America/New_York")) {
    withr::local_timezone(tz)
    for (k in seq_along(stamps)) {
      lit <- format(stamps[k], "%Y-%m-%d %H:%M:%OS6", tz = "UTC")
      ltz <- sf(sprintf("SELECT CONVERT_TIMEZONE('UTC', '%s'::TIMESTAMP_NTZ)::TIMESTAMP_LTZ AS v", lit))$v   # same moment
      tzv <- sf(sprintf("SELECT '%s +0000'::TIMESTAMP_TZ AS v", lit))$v
      expect_equal(as.numeric(tzv), as.numeric(stamps[k]), tolerance = 1e-6, info = paste(tz, lit))
      expect_s3_class(ltz, "POSIXct")
    }
  }
  off <- sf("SELECT '2026-10-14 12:34:56 +0530'::TIMESTAMP_TZ AS v")$v          # offset must not change the moment
  expect_equal(format(off, "%Y-%m-%d %H:%M:%S", tz = "UTC"), "2026-10-14 07:04:56")
})

test_that("egress: NULL in every column type is NA, not a value", {
  r <- sf("SELECT NULL::VARCHAR a, NULL::NUMBER(38,0) b, NULL::FLOAT c, NULL::BOOLEAN d, NULL::DATE e, NULL::TIME f, NULL::TIMESTAMP_NTZ g, NULL::TIMESTAMP_TZ h, NULL::BINARY i")
  expect_true(all(vapply(r, function(x) length(x) == 1L && is.na(x), logical(1))))
})

# ---- ROUND TRIP: write then read ------------------------------------------------------

test_that("round trip: strings, logicals, dates, times and timestamps come back identical", {
  for (tz in c("UTC", "Europe/London", "Pacific/Auckland")) {
    withr::local_timezone(tz)
    n <- length(strings)
    df <- data.frame(id = seq_len(n), txt = strings, l = rep(c(TRUE, FALSE, NA), length.out = n),
                     d = rep(dates, length.out = n), stringsAsFactors = FALSE)
    df$ts <- rep(stamps, length.out = n)
    df$tm <- rep(times, length.out = n)
    nm <- paste0("T_RT_", gsub("[^A-Z]", "", toupper(tz)))
    DBI::dbWriteTable(con, tbl_id(nm), df)
    b <- sf(sprintf("SELECT * FROM %s ORDER BY id", fq(nm)))
    expect_identical(b$txt, df$txt, info = tz)
    expect_identical(b$l, df$l, info = tz)
    expect_identical(b$d, df$d, info = tz)
    expect_equal(as.numeric(b$ts), as.numeric(df$ts), tolerance = 1e-6, info = paste("timestamps with R in", tz))
    expect_equal(as.numeric(b$tm), as.numeric(df$tm, units = "secs"), tolerance = 1e-9, info = tz)
  }
})

test_that("round trip: doubles come back within a few units in the last place; non-finite values exactly", {
  x <- c(doubles, NaN, Inf, -Inf, NA)
  DBI::dbWriteTable(con, tbl_id("T_RT_DBL"), data.frame(id = seq_along(x), x = x))
  b <- sf(sprintf("SELECT x FROM %s ORDER BY id", fq("T_RT_DBL")))$x
  finite <- is.finite(x)
  expect_true(all(ulps(b[finite], x[finite]) <= 4))
  expect_identical(is.nan(b), is.nan(x))
  expect_identical(b[is.infinite(x)], x[is.infinite(x)])
  expect_identical(is.na(b) & !is.nan(b), is.na(x) & !is.nan(x))
})

test_that("round trip: integers survive at every bigint setting", {
  x <- c(ints, NA)
  DBI::dbWriteTable(con, tbl_id("T_RT_INT"), data.frame(id = seq_along(x), x = x))
  for (setting in c("numeric", "integer", "character")) {
    c2 <- suppressMessages(DBI::dbConnect(Snowflake(), name = Sys.getenv("SKILIFT_PROFILE", "default"), bigint = setting))
    b <- DBI::dbGetQuery(c2, sprintf("SELECT x FROM %s ORDER BY id", fq("T_RT_INT")))[[1]]
    expect_identical(as.character(as.numeric(b)), as.character(as.numeric(x)), info = setting)
    DBI::dbDisconnect(c2)
  }
})

test_that("egress: BINARY arrives as the bytes (a blob), including empty and NULL", {
  r <- sf("SELECT TO_BINARY('DEADBEEF', 'HEX') AS a, TO_BINARY('', 'HEX') AS b, NULL::BINARY AS c, TO_BINARY('00FF10', 'HEX') AS d")
  expect_s3_class(r$a, "blob")
  expect_identical(unclass(r$a)[[1]], as.raw(c(0xde, 0xad, 0xbe, 0xef)))
  expect_identical(unclass(r$b)[[1]], raw(0))
  expect_null(unclass(r$c)[[1]])
  expect_identical(unclass(r$d)[[1]], as.raw(c(0x00, 0xff, 0x10)))
})

test_that("round trip: binary of every byte value comes back identical", {
  all_bytes <- as.raw(0:255)
  b <- blob::new_blob(list(all_bytes, raw(0), as.raw(0), NULL))
  DBI::dbWriteTable(con, tbl_id("T_RT_BIN"), data.frame(id = 1:4, b = I(b)))
  back <- sf(sprintf("SELECT b FROM %s ORDER BY id", fq("T_RT_BIN")))$b
  expect_identical(unclass(back), unclass(b))
})

test_that("field.types is honoured, and a column the table already has keeps its type", {
  df <- data.frame(a = c(1, 2), b = c("x", "y"), c = c(1L, 2L), stringsAsFactors = FALSE)
  DBI::dbWriteTable(con, tbl_id("T_FT"), df, field.types = c(a = "BIGINT", b = "VARCHAR(5)", c = "NUMBER(5,0)"))
  ty <- sf(sprintf("SELECT column_name, data_type, character_maximum_length AS len, numeric_precision AS p, numeric_scale AS s FROM %s.INFORMATION_SCHEMA.COLUMNS WHERE table_name = 'T_FT' ORDER BY ordinal_position", DB))
  expect_identical(ty$data_type, c("NUMBER", "TEXT", "NUMBER"))
  expect_equal(as.numeric(ty$len[2]), 5)
  expect_equal(as.numeric(ty$p), c(38, NA, 5))
  expect_equal(as.numeric(ty$s), c(0, NA, 0))
  expect_error(DBI::dbWriteTable(con, tbl_id("T_FT2"), df, field.types = c(nope = "BIGINT")), "does not have")
})
