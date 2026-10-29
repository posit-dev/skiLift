test_that("sf_fetch_all_as_arrow_stream converts JSON response to nanoarrow stream", {
  skip_if_not_installed("nanoarrow")

  con <- new("SnowflakeConnection",
    account = "test", user = "user", database = "db", schema = "sch",
    warehouse = "wh", role = "role",
    .auth = list(token = "tok", token_type = "KEYPAIR_JWT"),
    .state = .new_conn_state()
  )

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

  meta <- sf_parse_metadata(resp_body)
  stream <- sf_fetch_all_as_arrow_stream(con, resp_body, meta)
  expect_true(inherits(stream, "nanoarrow_array_stream"))

  df <- as.data.frame(stream)
  expect_equal(nrow(df), 2)
  expect_equal(ncol(df), 2)
})

test_that("sf_fetch_partition_as_arrow_stream returns stream for partition 0", {
  skip_if_not_installed("nanoarrow")

  con <- new("SnowflakeConnection",
    account = "test", user = "user", database = "db", schema = "sch",
    warehouse = "wh", role = "role",
    .auth = list(token = "tok", token_type = "KEYPAIR_JWT"),
    .state = .new_conn_state()
  )

  resp_body <- list(
    statementHandle = "h1",
    resultSetMetaData = list(
      numRows = 1L,
      format = "jsonv2",
      rowType = list(
        list(name = "VAL", type = "FIXED", nullable = FALSE, scale = 0L, precision = 10L)
      ),
      partitionInfo = list(list(rowCount = 1L))
    ),
    data = list(list("42"))
  )

  meta <- sf_parse_metadata(resp_body)
  stream <- sf_fetch_partition_as_arrow_stream(con, resp_body, meta, 0L)
  expect_true(inherits(stream, "nanoarrow_array_stream"))

  df <- as.data.frame(stream)
  expect_equal(nrow(df), 1)
  expect_equal(df$VAL, 42L)
})

test_that("sf_api_submit always uses jsonv2 format", {
  con <- new("SnowflakeConnection",
    account = "test", user = "user", database = "db", schema = "sch",
    warehouse = "wh", role = "role",
    .auth = list(token = "tok", token_type = "KEYPAIR_JWT"),
    .state = .new_conn_state()
  )

  called_format <- NULL
  mockr::with_mock(
    .sf_api_request_with_refresh = function(con, method, url, body = NULL, ...) {
      called_format <<- body$resultSetMetaData$format
      list(statementHandle = "h1", resultSetMetaData = list(
        numRows = 0L, rowType = list()
      ))
    },
    {
      sf_api_submit(con, "SELECT 1")
      expect_equal(called_format, "jsonv2")
    }
  )
})
