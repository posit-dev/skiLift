# Tests for optimised JSON parsing (.json_data_to_df matrix transpose approach)
# =============================================================================

# ---------------------------------------------------------------------------
# Helper to build a mock metadata object
# ---------------------------------------------------------------------------
.make_meta <- function(col_specs) {
  cols <- data.frame(
    name      = vapply(col_specs, `[[`, character(1), "name"),
    type      = vapply(col_specs, `[[`, character(1), "type"),
    nullable  = rep(TRUE, length(col_specs)),
    scale     = vapply(col_specs, function(s) as.integer(s$scale %||% 0L), integer(1)),
    precision = vapply(col_specs, function(s) as.integer(s$precision %||% 0L), integer(1)),
    stringsAsFactors = FALSE
  )
  list(
    num_rows         = 0L,
    num_partitions   = 1L,
    statement_handle = "test-handle",
    columns          = cols
  )
}


# ---------------------------------------------------------------------------
# Correctness: every Snowflake type round-trips through .json_data_to_df
# ---------------------------------------------------------------------------

test_that(".json_data_to_df handles integer fixed columns", {
  meta <- .make_meta(list(list(name = "ID", type = "fixed", scale = 0L, precision = 9L)))
  raw  <- list(list("1"), list("2"), list("42"), list(NULL))
  df   <- .json_data_to_df(raw, meta)

  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 4L)
  expect_type(df$ID, "integer")
  expect_equal(df$ID, c(1L, 2L, 42L, NA))
})

test_that(".json_data_to_df handles decimal fixed columns", {
  meta <- .make_meta(list(list(name = "PRICE", type = "fixed", scale = 2L)))
  raw  <- list(list("1.50"), list("2.75"), list(NULL))
  df   <- .json_data_to_df(raw, meta)

  expect_type(df$PRICE, "double")
  expect_equal(df$PRICE, c(1.50, 2.75, NA))
})

test_that(".json_data_to_df handles real/float columns", {
  meta <- .make_meta(list(list(name = "VAL", type = "real")))
  raw  <- list(list("3.14"), list(NULL), list("2.72"))
  df   <- .json_data_to_df(raw, meta)

  expect_type(df$VAL, "double")
  expect_equal(df$VAL, c(3.14, NA, 2.72))
})

test_that(".json_data_to_df handles text columns", {
  meta <- .make_meta(list(list(name = "NAME", type = "text")))
  raw  <- list(list("Alice"), list("Bob"), list(NULL))
  df   <- .json_data_to_df(raw, meta)

  expect_type(df$NAME, "character")
  expect_equal(df$NAME, c("Alice", "Bob", NA))
})

test_that(".json_data_to_df handles boolean columns", {
  meta <- .make_meta(list(list(name = "FLAG", type = "boolean")))
  raw  <- list(list("true"), list("false"), list(NULL), list("1"), list("0"))
  df   <- .json_data_to_df(raw, meta)

  expect_type(df$FLAG, "logical")
  expect_equal(df$FLAG, c(TRUE, FALSE, NA, TRUE, FALSE))
})

test_that(".json_data_to_df handles date columns", {
  meta <- .make_meta(list(list(name = "DT", type = "date")))
  raw  <- list(list("0"), list("19723"), list(NULL))
  df   <- .json_data_to_df(raw, meta)

  expect_s3_class(df$DT, "Date")
  expect_equal(df$DT[1], as.Date("1970-01-01"))
  expect_true(is.na(df$DT[3]))
})

test_that(".json_data_to_df handles timestamp_ntz columns", {
  epoch <- as.character(as.numeric(as.POSIXct("2024-01-15 10:30:00", tz = "UTC")))
  meta <- .make_meta(list(list(name = "TS", type = "timestamp_ntz")))
  raw  <- list(list(epoch), list(NULL))
  df   <- .json_data_to_df(raw, meta)

  expect_s3_class(df$TS, "POSIXct")
  expect_true(is.na(df$TS[2]))
})


# ---------------------------------------------------------------------------
# Multi-column: verify the matrix transpose handles mixed types
# ---------------------------------------------------------------------------

test_that(".json_data_to_df handles multiple columns of different types", {
  meta <- .make_meta(list(
    list(name = "ID",    type = "fixed",   scale = 0L),
    list(name = "NAME",  type = "text"),
    list(name = "SCORE", type = "real"),
    list(name = "ACTIVE", type = "boolean")
  ))
  raw <- list(
    list("1", "Alice", "95.5", "true"),
    list("2", "Bob",   "87.3", "false"),
    list("3", NULL,    NULL,   NULL)
  )
  df <- .json_data_to_df(raw, meta)

  expect_equal(nrow(df), 3L)
  expect_equal(ncol(df), 4L)
  expect_equal(df$ID,    c(1L, 2L, 3L))
  expect_equal(df$NAME,  c("Alice", "Bob", NA))
  expect_equal(df$SCORE, c(95.5, 87.3, NA))
  expect_equal(df$ACTIVE, c(TRUE, FALSE, NA))
})


# ---------------------------------------------------------------------------
# Edge cases
# ---------------------------------------------------------------------------

test_that(".json_data_to_df returns empty data.frame for zero columns", {
  meta <- list(columns = data.frame(
    name = character(0), type = character(0), nullable = logical(0),
    scale = integer(0), precision = integer(0), stringsAsFactors = FALSE
  ))
  df <- .json_data_to_df(list(list()), meta)
  expect_equal(nrow(df), 0L)
})

test_that(".json_data_to_df handles single-row data", {
  meta <- .make_meta(list(list(name = "X", type = "fixed", scale = 0L, precision = 9L)))
  raw  <- list(list("99"))
  df   <- .json_data_to_df(raw, meta)
  expect_equal(df$X, 99L)
})

test_that(".json_data_to_df handles all-NULL column", {
  meta <- .make_meta(list(list(name = "EMPTY", type = "text")))
  raw  <- list(list(NULL), list(NULL), list(NULL))
  df   <- .json_data_to_df(raw, meta)
  expect_equal(df$EMPTY, c(NA_character_, NA_character_, NA_character_))
})

test_that("a large whole-number column keeps one type whatever the values", {
  # NUMBER(38,0) is what a plain INTEGER column looks like to Snowflake. It
  # used to come back as integer or double depending on the values returned.
  meta <- .make_meta(list(list(name = "BIG", type = "fixed", scale = 0L, precision = 38L)))
  small <- .json_data_to_df(list(list("7")), meta)
  big   <- .json_data_to_df(list(list(as.character(.Machine$integer.max + 1))), meta)
  expect_type(small$BIG, "double")
  expect_type(big$BIG, "double")
  expect_equal(big$BIG, 2147483648)
})


# ---------------------------------------------------------------------------
# sf_parse_response integration
# ---------------------------------------------------------------------------

test_that("sf_parse_response handles a complete response body", {
  resp_body <- list(
    statementHandle = "h1",
    resultSetMetaData = list(
      numRows = 2L,
      format = "jsonv2",
      rowType = list(
        list(name = "ID", type = "FIXED", nullable = FALSE, scale = 0L, precision = 10L),
        list(name = "NAME", type = "TEXT", nullable = TRUE, scale = 0L, precision = 0L)
      ),
      partitionInfo = list(list(rowCount = 2L))
    ),
    data = list(
      list("1", "Alice"),
      list("2", "Bob")
    )
  )

  result <- sf_parse_response(resp_body)
  expect_equal(nrow(result$data), 2L)
  expect_equal(result$data$ID, c(1L, 2L))
  expect_equal(result$data$NAME, c("Alice", "Bob"))
})

test_that("sf_parse_response handles empty data", {
  resp_body <- list(
    statementHandle = "h2",
    resultSetMetaData = list(
      numRows = 0L,
      format = "jsonv2",
      rowType = list(
        list(name = "X", type = "TEXT", nullable = TRUE, scale = 0L, precision = 0L)
      ),
      partitionInfo = list()
    ),
    data = list()
  )

  result <- sf_parse_response(resp_body)
  expect_equal(nrow(result$data), 0L)
  expect_equal(names(result$data), "X")
})


# ---------------------------------------------------------------------------
# Large synthetic data -- correctness at scale
# ---------------------------------------------------------------------------

test_that(".json_data_to_df produces correct results for 10K rows", {
  n <- 10000L
  meta <- .make_meta(list(
    list(name = "ID",   type = "fixed", scale = 0L, precision = 9L),
    list(name = "VAL",  type = "real"),
    list(name = "FLAG", type = "boolean"),
    list(name = "TXT",  type = "text")
  ))

  raw <- lapply(seq_len(n), function(i) {
    list(
      as.character(i),
      as.character(runif(1)),
      if (i %% 7L == 0L) NULL else sample(c("true", "false"), 1L),
      if (i %% 13L == 0L) NULL else paste0("row_", i)
    )
  })

  df <- .json_data_to_df(raw, meta)

  expect_equal(nrow(df), n)
  expect_equal(ncol(df), 4L)
  expect_type(df$ID, "integer")
  expect_type(df$VAL, "double")
  expect_type(df$FLAG, "logical")
  expect_type(df$TXT, "character")

  expect_equal(df$ID[1], 1L)
  expect_equal(df$ID[n], as.integer(n))
  expect_true(is.na(df$FLAG[7]))
  expect_true(is.na(df$TXT[13]))
})
