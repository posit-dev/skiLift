# ---------------------------------------------------------------------------
# ADBC Backend Routing Tests
# ---------------------------------------------------------------------------

.test_conn <- function() {
  new("SnowflakeConnection",
    account = "test", user = "user", database = "db", schema = "sch",
    warehouse = "wh", role = "role",
    .auth = list(type = "pat", token = "tok", token_type = "PROGRAMMATIC_ACCESS_TOKEN"),
    .state = .new_conn_state()
  )
}

# ---------------------------------------------------------------------------
# State management
# ---------------------------------------------------------------------------

test_that(".new_conn_state initialises adbc to NULL", {
  state <- .new_conn_state()
  expect_null(state$adbc)
})

test_that(".has_adbc returns FALSE on fresh connection", {
  con <- .test_conn()
  expect_false(.has_adbc(con))
})

test_that(".has_adbc returns TRUE when adbc is populated", {
  con <- .test_conn()
  con@.state$adbc <- list(db = "db", con = "con")
  expect_true(.has_adbc(con))
})

# ---------------------------------------------------------------------------
# Auth mapping
# ---------------------------------------------------------------------------

test_that(".adbc_auth_args maps PAT auth correctly", {
  con <- .test_conn()
  args <- .adbc_auth_args(con)
  expect_equal(args[["adbc.snowflake.sql.auth_type"]], "auth_pat")
  expect_equal(args[["adbc.snowflake.sql.client_option.auth_token"]], "tok")
})

test_that(".adbc_auth_args maps JWT with key file path", {
  tmp <- tempfile(fileext = ".pem")
  writeLines("-----BEGIN PRIVATE KEY-----\nfake\n-----END PRIVATE KEY-----", tmp)
  on.exit(unlink(tmp))

  con <- .test_conn()
  con@.auth <- list(
    type = "jwt", token = "jwt_token",
    private_key_path = tmp,
    token_type = "KEYPAIR_JWT"
  )
  args <- .adbc_auth_args(con)
  expect_equal(args[["adbc.snowflake.sql.auth_type"]], "auth_jwt")
  expect_equal(args[["adbc.snowflake.sql.client_option.jwt_private_key"]], normalizePath(tmp))
})

test_that(".adbc_auth_args falls back to token when key file missing", {
  con <- .test_conn()
  con@.auth <- list(
    type = "jwt", token = "jwt_token",
    private_key_path = "/nonexistent/key.pem",
    token_type = "KEYPAIR_JWT"
  )
  args <- .adbc_auth_args(con)
  expect_equal(args[["adbc.snowflake.sql.auth_type"]], "auth_jwt")
  expect_equal(args[["adbc.snowflake.sql.client_option.auth_token"]], "jwt_token")
})

test_that(".adbc_auth_args maps token auth as PAT", {
  con <- .test_conn()
  con@.auth <- list(type = "token", token = "bearer_tok")
  args <- .adbc_auth_args(con)
  expect_equal(args[["adbc.snowflake.sql.auth_type"]], "auth_pat")
})

test_that(".adbc_auth_args maps oauth auth to auth_oauth", {
  con <- .test_conn()
  con@.auth <- list(type = "oauth", token = "spcs_oauth_token", token_type = "OAUTH")
  args <- .adbc_auth_args(con)
  expect_equal(args[["adbc.snowflake.sql.auth_type"]], "auth_oauth")
  expect_equal(args[["adbc.snowflake.sql.client_option.auth_token"]], "spcs_oauth_token")
})

test_that(".adbc_auth_args returns empty list for unknown auth type", {
  con <- .test_conn()
  con@.auth <- list(type = "unknown", token = "x")
  args <- .adbc_auth_args(con)
  expect_length(args, 0L)
})

# ---------------------------------------------------------------------------
# .ensure_adbc routing
# ---------------------------------------------------------------------------

test_that(".ensure_adbc returns NULL when backend is 'rest'", {
  con <- .test_conn()
  withr::with_options(list(skiLift.backend = "rest"), {
    expect_null(.ensure_adbc(con))
  })
})

test_that(".ensure_adbc returns cached value on second call", {
  con <- .test_conn()
  fake <- list(db = "db", con = "con")
  con@.state$adbc <- fake
  result <- .ensure_adbc(con)
  expect_identical(result, fake)
})

test_that(".ensure_adbc caches result in connection state", {
  con <- .test_conn()
  fake <- list(db = "fake_db", con = "fake_con")
  mockr::with_mock(
    .init_adbc_backend = function(conn) fake,
    {
      withr::with_options(list(skiLift.backend = "auto"), {
        result <- .ensure_adbc(con)
        expect_identical(result, fake)
        expect_identical(con@.state$adbc, fake)
      })
    }
  )
})

test_that(".ensure_adbc returns NULL when packages unavailable", {
  con <- .test_conn()
  mockr::with_mock(
    .init_adbc_backend = function(conn) NULL,
    {
      withr::with_options(list(skiLift.backend = "auto"), {
        expect_null(.ensure_adbc(con))
      })
    }
  )
})

test_that(".ensure_adbc caches failed init and does not retry", {
  con <- .test_conn()
  init_count <- 0L
  mockr::with_mock(
    .init_adbc_backend = function(conn) { init_count <<- init_count + 1L; NULL },
    {
      withr::with_options(list(skiLift.backend = "auto"), {
        expect_null(.ensure_adbc(con))
        expect_null(.ensure_adbc(con))
        expect_equal(init_count, 1L)
        expect_identical(con@.state$adbc, "failed")
      })
    }
  )
})

# ---------------------------------------------------------------------------
# Write routing (.insert_data)
# ---------------------------------------------------------------------------

test_that(".insert_data routes to literal for small data in auto mode", {
  con <- .test_conn()
  captured_sql <- character(0)
  mockr::with_mock(
    sf_api_submit = function(conn, sql, ...) {
      captured_sql <<- c(captured_sql, sql)
      list()
    },
    {
      withr::with_envvar(c(SNOWFLAKE_HOST = NA), {
        withr::with_options(list(
          skiLift.upload_method = "auto",
          skiLift.bulk_write_threshold = 1000L,
          skiLift.adbc_write_threshold = 1000L
        ), {
          df <- data.frame(x = 1:10)
          .insert_data(con, '"T"', df)
        })
      })
    }
  )
  expect_length(captured_sql, 1L)
  expect_no_match(captured_sql, "\\?")
})

test_that(".insert_data attempts ADBC for large data in auto mode (outside Workspace)", {
  con <- .test_conn()
  adbc_called <- FALSE
  fake_adbc <- list(db = "db", con = "con")
  con@.state$adbc <- fake_adbc

  mockr::with_mock(
    .adbc_write_table = function(adbc, table_name, df, ...) {
      adbc_called <<- TRUE
      invisible(TRUE)
    },
    {
      withr::with_envvar(c(SNOWFLAKE_HOST = NA), {
        withr::with_options(list(
          skiLift.upload_method = "auto",
          skiLift.bulk_write_threshold = 10L,
          skiLift.adbc_write_threshold = 10L
        ), {
          df <- data.frame(a = 1:5, b = 6:10, c = 11:15)
          .insert_data(con, '"T"', df)
        })
      })
    }
  )
  expect_true(adbc_called)
})

test_that(".insert_data stays on literal when below threshold in auto mode", {
  con <- .test_conn()
  fake_adbc <- list(db = "db", con = "con")
  con@.state$adbc <- fake_adbc

  adbc_bulk_called <- FALSE
  adbc_sql_called <- FALSE
  rest_called <- FALSE
  mockr::with_mock(
    .adbc_write_table = function(...) { adbc_bulk_called <<- TRUE },
    .adbc_execute_sql = function(...) { adbc_sql_called <<- TRUE; 0L },
    sf_api_submit = function(...) { rest_called <<- TRUE; list() },
    {
      withr::with_envvar(c(SNOWFLAKE_HOST = NA), {
        withr::with_options(list(
          skiLift.upload_method = "auto",
          skiLift.bulk_write_threshold = 999999L,
          skiLift.adbc_write_threshold = 999999L
        ), {
          df <- data.frame(x = 1:10)
          .insert_data(con, '"T"', df)
        })
      })
    }
  )
  expect_false(adbc_bulk_called)
  expect_true(adbc_sql_called)
})

test_that(".insert_data falls back to literal when method='adbc' but no backend", {
  con <- .test_conn()
  captured_sql <- character(0)
  mockr::with_mock(
    .ensure_adbc = function(conn) NULL,
    sf_api_submit = function(conn, sql, ...) {
      captured_sql <<- c(captured_sql, sql)
      list()
    },
    {
      withr::with_options(list(skiLift.upload_method = "adbc"), {
        df <- data.frame(x = 1L)
        expect_warning(.insert_data(con, '"T"', df), "ADBC")
      })
    }
  )
  expect_length(captured_sql, 1L)
})

test_that(".insert_data respects method='bind' override with deprecation warning", {
  con <- .test_conn()
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
  expect_length(captured_sql, 1L)
  expect_match(captured_sql, "\\?")
})

test_that(".insert_data_adbc strips DBI quoting from table_id", {
  written_name <- NULL
  mockr::with_mock(
    .adbc_write_table = function(adbc, table_name, df, ...) {
      written_name <<- table_name
      invisible(TRUE)
    },
    {
      .insert_data_adbc(list(), '"MY_TABLE"', data.frame(x = 1))
    }
  )
  expect_equal(written_name, "MY_TABLE")
})

# ---------------------------------------------------------------------------
# Snowpark write routing
# ---------------------------------------------------------------------------

test_that(".insert_data routes to Snowpark in Workspace auto mode for large data", {
  con <- .test_conn()
  snowpark_called <- FALSE

  mockr::with_mock(
    .snowpark_write_available = function() TRUE,
    .insert_data_snowpark = function(conn, table_id, df) {
      snowpark_called <<- TRUE
      invisible(TRUE)
    },
    {
      withr::with_envvar(c(SNOWFLAKE_HOST = "fake-spcs.snowflakecomputing.app"), {
        withr::with_options(list(
          skiLift.upload_method = "auto",
          skiLift.bulk_write_threshold = 10L
        ), {
          df <- data.frame(a = 1:5, b = 6:10, c = 11:15)
          .insert_data(con, '"T"', df)
        })
      })
    }
  )
  expect_true(snowpark_called)
})

test_that(".insert_data falls back to ADBC in Workspace when Snowpark unavailable", {
  con <- .test_conn()
  fake_adbc <- list(db = "db", con = "con")
  con@.state$adbc <- fake_adbc
  adbc_called <- FALSE

  mockr::with_mock(
    .snowpark_write_available = function() FALSE,
    .adbc_write_table = function(adbc, table_name, df, ...) {
      adbc_called <<- TRUE
      invisible(TRUE)
    },
    {
      withr::with_envvar(c(SNOWFLAKE_HOST = "fake-spcs.snowflakecomputing.app"), {
        withr::with_options(list(
          skiLift.upload_method = "auto",
          skiLift.bulk_write_threshold = 10L
        ), {
          df <- data.frame(a = 1:5, b = 6:10, c = 11:15)
          .insert_data(con, '"T"', df)
        })
      })
    }
  )
  expect_true(adbc_called)
})

test_that(".insert_data routes to Snowpark when method='snowpark'", {
  con <- .test_conn()
  snowpark_called <- FALSE

  mockr::with_mock(
    .insert_data_snowpark = function(conn, table_id, df) {
      snowpark_called <<- TRUE
      invisible(TRUE)
    },
    {
      withr::with_envvar(c(SNOWFLAKE_HOST = NA), {
        withr::with_options(list(skiLift.upload_method = "snowpark"), {
          df <- data.frame(x = 1L)
          .insert_data(con, '"T"', df)
        })
      })
    }
  )
  expect_true(snowpark_called)
})

test_that(".insert_data does NOT use Snowpark outside Workspace in auto mode", {
  con <- .test_conn()
  snowpark_called <- FALSE
  adbc_called <- FALSE
  fake_adbc <- list(db = "db", con = "con")
  con@.state$adbc <- fake_adbc

  mockr::with_mock(
    .snowpark_write_available = function() TRUE,
    .insert_data_snowpark = function(...) { snowpark_called <<- TRUE; invisible(TRUE) },
    .adbc_write_table = function(adbc, table_name, df, ...) {
      adbc_called <<- TRUE
      invisible(TRUE)
    },
    {
      withr::with_envvar(c(SNOWFLAKE_HOST = NA), {
        withr::with_options(list(
          skiLift.upload_method = "auto",
          skiLift.bulk_write_threshold = 10L
        ), {
          df <- data.frame(a = 1:5, b = 6:10, c = 11:15)
          .insert_data(con, '"T"', df)
        })
      })
    }
  )
  expect_false(snowpark_called)
  expect_true(adbc_called)
})

test_that(".snowpark_write_available returns FALSE when reticulate missing", {
  mockr::with_mock(
    .has_reticulate = function() FALSE,
    {
      expect_false(.snowpark_write_available())
    }
  )
})

test_that(".ensure_snowpark_session caches failed init and does not retry", {
  con <- .test_conn()
  init_count <- 0L

  mockr::with_mock(
    .has_reticulate = function() TRUE,
    .get_or_create_snowpark_session = function(conn) {
      init_count <<- init_count + 1L
      stop("snowpark unavailable")
    },
    {
      result <- suppressWarnings(.ensure_snowpark_session(con))
      expect_null(result)
      expect_identical(con@.state$snowpark_session, "failed")

      result2 <- .ensure_snowpark_session(con)
      expect_null(result2)
      expect_equal(init_count, 1L)
    }
  )
})

test_that(".ensure_snowpark_session returns NULL when reticulate missing", {
  con <- .test_conn()
  mockr::with_mock(
    .has_reticulate = function() FALSE,
    {
      result <- .ensure_snowpark_session(con)
      expect_null(result)
      expect_identical(con@.state$snowpark_session, "failed")
    }
  )
})

# ---------------------------------------------------------------------------
# Cleanup
# ---------------------------------------------------------------------------

test_that("dbDisconnect releases ADBC state", {
  con <- .test_conn()
  con@.state$adbc <- list(db = "db", con = "con")

  mockr::with_mock(
    .adbc_cleanup = function(adbc) invisible(),
    .on_connection_closed = function(conn) invisible(),
    {
      dbDisconnect(con)
    }
  )
  expect_null(con@.state$adbc)
  expect_false(dbIsValid(con))
})

test_that("dbDisconnect works when no ADBC backend present", {
  con <- .test_conn()
  mockr::with_mock(
    .on_connection_closed = function(conn) invisible(),
    {
      expect_true(dbDisconnect(con))
      expect_false(dbIsValid(con))
    }
  )
})

# ---------------------------------------------------------------------------
# Option defaults
# ---------------------------------------------------------------------------

test_that("default options are set correctly (non-Workspace)", {
  on_load <- skiLift:::.onLoad
  withr::with_envvar(c(SNOWFLAKE_HOST = NA), {
    withr::with_options(list(
      skiLift.backend = NULL,
      skiLift.adbc_write_threshold = NULL,
      skiLift.bulk_write_threshold = NULL,
      skiLift.upload_method = NULL
    ), {
      expect_null(getOption("skiLift.backend"))
      on_load("", "skiLift")
      expect_equal(getOption("skiLift.backend"), "auto")
      expect_equal(getOption("skiLift.bulk_write_threshold"), 50000L)
      expect_equal(getOption("skiLift.adbc_write_threshold"), 50000L)
      expect_equal(getOption("skiLift.upload_method"), "auto")
    })
  })
})

test_that("Workspace environment raises bulk write threshold", {
  on_load <- skiLift:::.onLoad
  withr::with_envvar(c(SNOWFLAKE_HOST = "fake-spcs-host.snowflakecomputing.app"), {
    withr::with_options(list(
      skiLift.adbc_write_threshold = NULL,
      skiLift.bulk_write_threshold = NULL
    ), {
      on_load("", "skiLift")
      expect_equal(getOption("skiLift.bulk_write_threshold"), 200000L)
      expect_equal(getOption("skiLift.adbc_write_threshold"), 200000L)
    })
  })
})
