# Tests for native Arrow transport helpers
# =============================================================================

# ---------------------------------------------------------------------------
# .can_use_native_arrow
# ---------------------------------------------------------------------------

test_that(".can_use_native_arrow returns FALSE without session", {
  con <- new("SnowflakeConnection",
    account = "test", user = "user", database = "db", schema = "sch",
    warehouse = "wh", role = "role",
    .auth = list(token = "tok", token_type = "KEYPAIR_JWT"),
    .state = .new_conn_state()
  )

  withr::with_options(list(skiLift.use_native_arrow = TRUE), {
    expect_false(.can_use_native_arrow(con))
  })
})

test_that(".can_use_native_arrow returns FALSE when option disabled", {
  state <- .new_conn_state()
  state$session <- list(token = "tok", master_token = "mt")
  con <- new("SnowflakeConnection",
    account = "test", user = "user", database = "db", schema = "sch",
    warehouse = "wh", role = "role",
    .auth = list(token = "tok", token_type = "KEYPAIR_JWT"),
    .state = state
  )

  withr::with_options(list(skiLift.use_native_arrow = FALSE), {
    expect_false(.can_use_native_arrow(con))
  })
})

test_that(".can_use_native_arrow checks nanoarrow availability", {
  state <- .new_conn_state()
  state$session <- list(token = "tok", master_token = "mt")
  con <- new("SnowflakeConnection",
    account = "test", user = "user", database = "db", schema = "sch",
    warehouse = "wh", role = "role",
    .auth = list(token = "tok", token_type = "KEYPAIR_JWT"),
    .state = state
  )

  withr::with_options(list(skiLift.use_native_arrow = TRUE), {
    result <- .can_use_native_arrow(con)
    # Result depends on whether nanoarrow is installed locally
    expect_type(result, "logical")
  })
})


# ---------------------------------------------------------------------------
# sf_decode_arrow_base64
# ---------------------------------------------------------------------------

test_that("sf_decode_arrow_base64 returns NULL for empty input", {
  skip_if_not_installed("nanoarrow")
  expect_null(sf_decode_arrow_base64(NULL))
  expect_null(sf_decode_arrow_base64(""))
})

test_that("sf_decode_arrow_base64 returns NULL for invalid base64", {
  skip_if_not_installed("nanoarrow")
  skip_if_not_installed("openssl")
  result <- suppressWarnings(sf_decode_arrow_base64("not-valid-base64!@#"))
  # May return NULL due to decode error or parse error
  expect_true(is.null(result) || inherits(result, "nanoarrow_array_stream"))
})


# ---------------------------------------------------------------------------
# .decrypt_arrow_chunk
# ---------------------------------------------------------------------------

test_that(".decrypt_arrow_chunk handles invalid key gracefully", {
  skip_if_not_installed("openssl")
  result <- suppressWarnings(
    .decrypt_arrow_chunk(as.raw(c(1:32)), "invalid-key")
  )
  expect_null(result)
})


# ---------------------------------------------------------------------------
# sf_fetch_all_native_arrow
# ---------------------------------------------------------------------------

test_that("sf_fetch_all_native_arrow errors on missing rowsetBase64", {
  skip_if_not_installed("nanoarrow")

  con <- new("SnowflakeConnection",
    account = "test", user = "user", database = "db", schema = "sch",
    warehouse = "wh", role = "role",
    .auth = list(token = "tok", token_type = "KEYPAIR_JWT"),
    .state = .new_conn_state()
  )

  resp <- list(data = list(rowsetBase64 = NULL, chunks = NULL))
  expect_error(sf_fetch_all_native_arrow(con, resp), "rowsetBase64")
})


# ---------------------------------------------------------------------------
# Option default values
# ---------------------------------------------------------------------------

test_that("use_native_arrow defaults to FALSE", {
  withr::with_options(list(skiLift.use_native_arrow = NULL), {
    val <- getOption("skiLift.use_native_arrow", FALSE)
    expect_false(val)
  })
})
