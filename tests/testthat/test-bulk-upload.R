# ---------------------------------------------------------------------------
# Legacy literal INSERT path
# ---------------------------------------------------------------------------

test_that(".format_value handles all types", {
  expect_equal(.format_value(NA), "NULL")
  expect_equal(.format_value(TRUE), "TRUE")
  expect_equal(.format_value(FALSE), "FALSE")
  expect_equal(.format_value(42L), "42")
  expect_equal(.format_value(3.14), "3.14")
  expect_equal(.format_value("hello"), "'hello'")
  expect_equal(.format_value("it's"), "'it''s'")
  expect_equal(.format_value(as.Date("2024-01-15")), "'2024-01-15'")
})

test_that(".insert_data_literal generates named-column INSERT SQL", {
  con <- new("SnowflakeConnection",
    account = "test", user = "user", database = "db", schema = "sch",
    warehouse = "wh", role = "role",
    .auth = list(token = "tok", token_type = "KEYPAIR_JWT"),
    .state = .new_conn_state()
  )

  captured_sql <- character(0)
  mockr::with_mock(
    sf_api_submit = function(conn, sql, ...) {
      captured_sql <<- c(captured_sql, sql)
      list()
    },
    {
      df <- data.frame(id = 1:2, name = c("a", "b"), stringsAsFactors = FALSE)
      .insert_data_literal(con, '"MY_TABLE"', df)
    }
  )

  expect_length(captured_sql, 1L)
  expect_match(captured_sql, '"id"', fixed = TRUE)
  expect_match(captured_sql, '"name"', fixed = TRUE)
  expect_match(captured_sql, "INSERT INTO")
})

test_that(".insert_data_literal respects configurable batch size", {
  con <- new("SnowflakeConnection",
    account = "test", user = "user", database = "db", schema = "sch",
    warehouse = "wh", role = "role",
    .auth = list(token = "tok", token_type = "KEYPAIR_JWT"),
    .state = .new_conn_state()
  )

  call_count <- 0L
  mockr::with_mock(
    sf_api_submit = function(conn, sql, ...) {
      call_count <<- call_count + 1L
      list()
    },
    {
      withr::with_options(list(skiLift.insert_batch_size = 3L), {
        df <- data.frame(x = 1:10)
        .insert_data_literal(con, '"TBL"', df)
      })
    }
  )

  expect_equal(call_count, 4L)
})


# ---------------------------------------------------------------------------
# Bind-parameter INSERT path (deprecated -- kept for backwards compatibility)
# ---------------------------------------------------------------------------

test_that(".r_col_to_sf_bind_type maps R types correctly", {
  expect_equal(.r_col_to_sf_bind_type(42L), "FIXED")
  expect_equal(.r_col_to_sf_bind_type(3.14), "REAL")
  expect_equal(.r_col_to_sf_bind_type("hello"), "TEXT")
  expect_equal(.r_col_to_sf_bind_type(TRUE), "BOOLEAN")
  expect_equal(.r_col_to_sf_bind_type(as.Date("2024-06-15")), "TEXT")
  expect_equal(.r_col_to_sf_bind_type(as.POSIXct("2024-06-15")), "TEXT")
})

test_that(".col_to_string_values converts columns vectorised", {
  vals <- .col_to_string_values(c(1L, 2L, NA_integer_))
  expect_equal(vals[[1]], "1")
  expect_equal(vals[[2]], "2")
  expect_null(vals[[3]])

  vals_bool <- .col_to_string_values(c(TRUE, FALSE, NA))
  expect_equal(vals_bool[[1]], "TRUE")
  expect_equal(vals_bool[[2]], "FALSE")
  expect_null(vals_bool[[3]])

  vals_date <- .col_to_string_values(as.Date(c("2024-01-15", NA)))
  expect_equal(vals_date[[1]], "2024-01-15")
  expect_null(vals_date[[2]])
})

test_that(".df_slice_to_bindings numbers parameters sequentially (row-major)", {
  df <- data.frame(a = 1:3, b = c("x", "y", "z"), stringsAsFactors = FALSE)
  bindings <- .df_slice_to_bindings(df, 1L, 2L)

  expect_equal(length(bindings), 4L)
  expect_equal(names(bindings), c("1", "2", "3", "4"))
  expect_equal(bindings[["1"]]$type, "FIXED")
  expect_equal(bindings[["1"]]$value, "1")
  expect_equal(bindings[["2"]]$type, "TEXT")
  expect_equal(bindings[["2"]]$value, "x")
  expect_equal(bindings[["3"]]$value, "2")
  expect_equal(bindings[["4"]]$value, "y")
})

test_that(".insert_data_bind generates INSERT with ? placeholders", {
  con <- new("SnowflakeConnection",
    account = "test", user = "user", database = "db", schema = "sch",
    warehouse = "wh", role = "role",
    .auth = list(token = "tok", token_type = "KEYPAIR_JWT"),
    .state = .new_conn_state()
  )

  captured_sql <- character(0)
  captured_bindings <- list()
  mockr::with_mock(
    sf_api_submit = function(conn, sql, bindings = NULL, ...) {
      captured_sql <<- c(captured_sql, sql)
      captured_bindings <<- c(captured_bindings, list(bindings))
      list()
    },
    {
      df <- data.frame(id = 1:2, name = c("a", "b"), stringsAsFactors = FALSE)
      .insert_data_bind(con, '"MY_TABLE"', df, batch_size = 100L)
    }
  )

  expect_length(captured_sql, 1L)
  expect_match(captured_sql, "\\?")
  expect_match(captured_sql, "INSERT INTO")
  # Flat row-major bindings: nrows * ncols = 2 * 2 = 4
  expect_equal(length(captured_bindings[[1L]]), 4L)
})

test_that(".insert_data_bind respects batch_size", {
  con <- new("SnowflakeConnection",
    account = "test", user = "user", database = "db", schema = "sch",
    warehouse = "wh", role = "role",
    .auth = list(token = "tok", token_type = "KEYPAIR_JWT"),
    .state = .new_conn_state()
  )

  call_count <- 0L
  mockr::with_mock(
    sf_api_submit = function(conn, sql, ...) {
      call_count <<- call_count + 1L
      list()
    },
    {
      df <- data.frame(x = 1:10)
      .insert_data_bind(con, '"TBL"', df, batch_size = 3L)
    }
  )

  expect_equal(call_count, 4L)
})

test_that(".insert_data routes to bind path with deprecation warning", {
  con <- new("SnowflakeConnection",
    account = "test", user = "user", database = "db", schema = "sch",
    warehouse = "wh", role = "role",
    .auth = list(token = "tok", token_type = "KEYPAIR_JWT"),
    .state = .new_conn_state()
  )

  captured_sql <- character(0)
  mockr::with_mock(
    sf_api_submit = function(conn, sql, ...) {
      captured_sql <<- c(captured_sql, sql)
      list()
    },
    {
      withr::with_options(list(skiLift.upload_method = "bind"), {
        df <- data.frame(x = 1L)
        expect_warning(.insert_data(con, '"T"', df), "deprecated")
      })
    }
  )

  expect_match(captured_sql, "\\?")
})

test_that(".insert_data routes to literal path when configured", {
  con <- new("SnowflakeConnection",
    account = "test", user = "user", database = "db", schema = "sch",
    warehouse = "wh", role = "role",
    .auth = list(token = "tok", token_type = "KEYPAIR_JWT"),
    .state = .new_conn_state()
  )

  captured_sql <- character(0)
  mockr::with_mock(
    sf_api_submit = function(conn, sql, ...) {
      captured_sql <<- c(captured_sql, sql)
      list()
    },
    {
      withr::with_options(list(skiLift.upload_method = "literal"), {
        df <- data.frame(x = 1L)
        .insert_data(con, '"T"', df)
      })
    }
  )

  expect_no_match(captured_sql, "\\?")
})
