test_that("dbplyr_edition returns 2 for SnowflakeConnection", {
  skip_if_not_installed("dbplyr")
  con <- new("SnowflakeConnection",
    account = "test", user = "user", database = "mydb", schema = "public",
    warehouse = "wh", role = "role", .auth = list(), .state = .new_conn_state()
  )
  expect_equal(dbplyr_edition.SnowflakeConnection(con), 2L)
})

test_that("sql_translation returns Snowflake translations", {
  skip_if_not_installed("dbplyr")
  con <- new("SnowflakeConnection",
    account = "test", user = "user", database = "mydb", schema = "public",
    warehouse = "wh", role = "role", .auth = list(), .state = .new_conn_state()
  )
  trans <- suppressWarnings(sql_translation.SnowflakeConnection(con))
  expect_s3_class(trans, "sql_variant")
})

test_that("db_connection_describe formats correctly", {
  skip_if_not_installed("dbplyr")
  con <- new("SnowflakeConnection",
    account = "acme", user = "user", database = "analytics",
    schema = "raw", warehouse = "wh", role = "role",
    .auth = list(), .state = .new_conn_state()
  )
  desc <- db_connection_describe.SnowflakeConnection(con)
  expect_match(desc, "Snowflake acme")
  expect_match(desc, "analytics.raw")
})

test_that("sql_query_save generates CREATE TABLE AS", {
  skip_if_not_installed("dbplyr")
  con <- new("SnowflakeConnection",
    account = "test", user = "user", database = "mydb", schema = "public",
    warehouse = "wh", role = "role", .auth = list(), .state = .new_conn_state()
  )
  sql <- sql_query_save.SnowflakeConnection(
    con, dbplyr::sql("SELECT 1"), dbplyr::ident("my_tbl"), temporary = TRUE
  )
  expect_match(as.character(sql), "CREATE TEMPORARY TABLE")
})

test_that("Snowflake paste0 translates to ARRAY_TO_STRING", {
  skip_if_not_installed("dbplyr")
  skip_if_not_installed("dplyr")
  lf <- dbplyr::lazy_frame(a = "x", b = "y", con = dbplyr::simulate_snowflake())
  out <- dbplyr::sql_render(dplyr::mutate(lf, c = paste0(a, b)))
  expect_match(as.character(out), "ARRAY_TO_STRING", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# skiLift-specific Snowflake scalar translations
# ---------------------------------------------------------------------------

.make_test_con <- function() {
  new("SnowflakeConnection",
    account = "test", user = "user", database = "mydb", schema = "public",
    warehouse = "wh", role = "role", .auth = list(), .state = .new_conn_state()
  )
}

# Suppress the harmless "missing window variants" warning from sql_variant()
.get_translation <- function(con) {
  suppressWarnings(sql_translation.SnowflakeConnection(con))
}

test_that("Semi-structured scalar translations produce correct SQL", {
  skip_if_not_installed("dbplyr")
  con   <- .make_test_con()
  trans <- .get_translation(con)

  expect_match(
    as.character(trans$scalar$parse_json(dbplyr::ident("col"))),
    "PARSE_JSON", fixed = TRUE
  )
  expect_match(
    as.character(trans$scalar$try_parse_json(dbplyr::ident("col"))),
    "TRY_PARSE_JSON", fixed = TRUE
  )
  expect_match(
    as.character(trans$scalar$typeof(dbplyr::ident("col"))),
    "TYPEOF", fixed = TRUE
  )
  expect_match(
    as.character(trans$scalar$is_object(dbplyr::ident("col"))),
    "IS_OBJECT", fixed = TRUE
  )
  expect_match(
    as.character(trans$scalar$is_array(dbplyr::ident("col"))),
    "IS_ARRAY", fixed = TRUE
  )
  expect_match(
    as.character(trans$scalar$is_integer(dbplyr::ident("col"))),
    "IS_INTEGER", fixed = TRUE
  )
})

test_that("Array scalar translations produce correct SQL", {
  skip_if_not_installed("dbplyr")
  con   <- .make_test_con()
  trans <- .get_translation(con)

  expect_match(
    as.character(trans$scalar$array_size(dbplyr::ident("col"))),
    "ARRAY_SIZE", fixed = TRUE
  )

  # array_contains swaps arg order: R (arr, val) -> SQL ARRAY_CONTAINS(val, arr)
  result <- as.character(
    trans$scalar$array_contains(dbplyr::ident("arr"), dbplyr::ident("v"))
  )
  expect_match(result, "ARRAY_CONTAINS", fixed = TRUE)
  expect_match(result, '"v", "arr"', fixed = TRUE)

  expect_match(
    as.character(trans$scalar$array_slice(dbplyr::ident("a"), 0L, 5L)),
    "ARRAY_SLICE", fixed = TRUE
  )
})

test_that("regexp_substr scalar translation handles defaults", {
  skip_if_not_installed("dbplyr")
  con   <- .make_test_con()
  trans <- .get_translation(con)

  result <- as.character(trans$scalar$regexp_substr(dbplyr::ident("x"), "pat"))
  expect_match(result, "REGEXP_SUBSTR", fixed = TRUE)
  expect_match(result, "'pat'", fixed = TRUE)

  result_custom <- as.character(
    trans$scalar$regexp_substr(dbplyr::ident("x"), "pat", 1L, 2L, "i")
  )
  expect_match(result_custom, "REGEXP_SUBSTR", fixed = TRUE)
  expect_match(result_custom, "'i'", fixed = TRUE)
})

test_that("hash scalar translation handles variadic args", {
  skip_if_not_installed("dbplyr")
  con   <- .make_test_con()
  trans <- .get_translation(con)

  result <- as.character(trans$scalar$hash(dbplyr::ident("a"), dbplyr::ident("b")))
  expect_match(result, "HASH(", fixed = TRUE)
  expect_match(result, '"a"', fixed = TRUE)
  expect_match(result, '"b"', fixed = TRUE)
})

# ---------------------------------------------------------------------------
# skiLift-specific Snowflake aggregate translations
# ---------------------------------------------------------------------------

test_that("Semi-structured aggregate translations produce correct SQL", {
  skip_if_not_installed("dbplyr")
  con   <- .make_test_con()
  trans <- .get_translation(con)

  result <- as.character(
    trans$aggregate$object_agg(dbplyr::ident("k"), dbplyr::ident("v"))
  )
  expect_match(result, "OBJECT_AGG", fixed = TRUE)
  expect_match(result, '"k"', fixed = TRUE)
  expect_match(result, '"v"', fixed = TRUE)

  expect_match(
    as.character(trans$aggregate$array_agg(dbplyr::ident("col"))),
    "ARRAY_AGG", fixed = TRUE
  )
  expect_match(
    as.character(trans$aggregate$array_unique_agg(dbplyr::ident("col"))),
    "ARRAY_UNIQUE_AGG", fixed = TRUE
  )
})

test_that("Approximate aggregate translations produce correct SQL", {
  skip_if_not_installed("dbplyr")
  con   <- .make_test_con()
  trans <- .get_translation(con)

  expect_match(
    as.character(trans$aggregate$approx_count_distinct(dbplyr::ident("col"))),
    "APPROX_COUNT_DISTINCT", fixed = TRUE
  )

  result <- as.character(
    trans$aggregate$approx_percentile(dbplyr::ident("col"), 0.95)
  )
  expect_match(result, "APPROX_PERCENTILE", fixed = TRUE)
  expect_match(result, "0.95", fixed = TRUE)
})

test_that("mode_val aggregate translates to SQL MODE()", {
  skip_if_not_installed("dbplyr")
  con   <- .make_test_con()
  trans <- .get_translation(con)

  result <- as.character(trans$aggregate$mode_val(dbplyr::ident("col")))
  expect_match(result, "MODE(", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# Inherited dbplyr translations still work after extension
# ---------------------------------------------------------------------------

test_that("Inherited dbplyr Snowflake translations remain functional", {
  skip_if_not_installed("dbplyr")
  con   <- .make_test_con()
  trans <- .get_translation(con)

  expect_s3_class(trans, "sql_variant")

  # floor_date -> DATE_TRUNC (inherited from dbplyr Snowflake backend)
  floor_sql <- as.character(
    dbplyr::translate_sql(floor_date(x, "week"), con = dbplyr::simulate_snowflake())
  )
  expect_match(floor_sql, "DATE_TRUNC", fixed = TRUE)

  # ifelse -> IFF / CASE WHEN (inherited)
  if_sql <- as.character(
    dbplyr::translate_sql(ifelse(x, y, z), con = dbplyr::simulate_snowflake())
  )
  expect_match(if_sql, "CASE WHEN|IFF")
})
