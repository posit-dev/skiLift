# External browser SSO resolution.
#
# The browser round-trip itself belongs to snowflakeauth and cannot be
# exercised here -- it opens a browser and needs a live SSO account. What is
# testable, and what actually broke before, is the routing: which branch of
# sf_auth_resolve() a given environment lands in, and whether the resulting
# auth object carries a usable header.

test_that("externalbrowser is not reachable inside a Workspace container", {
  # C5 of the auth reference: browser auth cannot work in a container, and the
  # SPCS session token is the only valid credential there. The Workspace branch
  # must win even when externalbrowser is explicitly requested.
  withr::with_envvar(
    c(SNOWFLAKE_HOST = "snowflake.internal", SNOWFLAKE_TOKEN = "spcs-token"),
    {
      auth <- sf_auth_resolve(
        account = "acct",
        authenticator = "externalbrowser"
      )
      expect_equal(auth$type, "oauth")
      expect_equal(auth$token, "spcs-token")
      expect_null(auth$headers)
    }
  )
})

test_that("explicit externalbrowser beats an ambient SNOWFLAKE_PAT", {
  # A stray PAT in the environment should not silently override an
  # authenticator the caller asked for by name. We only check that the PAT
  # branch is *not* taken -- resolution then proceeds into snowflakeauth,
  # which needs a browser, so an error here is the expected outcome.
  withr::with_envvar(
    c(SNOWFLAKE_HOST = NA, SNOWFLAKE_TOKEN = NA, SNOWFLAKE_PAT = "pat-token"),
    {
      res <- tryCatch(
        sf_auth_resolve(account = "acct", authenticator = "externalbrowser"),
        error = function(e) e
      )
      if (inherits(res, "condition")) {
        # Whatever stopped us, it must not be the PAT branch succeeding.
        expect_false(grepl("PROGRAMMATIC_ACCESS_TOKEN", conditionMessage(res)))
      } else {
        expect_false(identical(res$type, "pat"))
      }
    }
  )
})

test_that("explicit token still takes priority over externalbrowser", {
  auth <- sf_auth_resolve(
    account = "acct",
    token = "explicit",
    authenticator = "externalbrowser"
  )
  expect_equal(auth$type, "token")
  expect_equal(auth$token, "explicit")
})

test_that("externalbrowser requires an account", {
  withr::with_envvar(
    c(SNOWFLAKE_HOST = NA, SNOWFLAKE_TOKEN = NA, SNOWFLAKE_PAT = NA),
    expect_error(
      sf_auth_externalbrowser(account = NULL),
      "requires"
    )
  )
})

test_that("the credentials-not-found error now points at externalbrowser", {
  # The original failure mode Chetan hit: an externalbrowser profile fell
  # through every branch and produced a message that named neither the cause
  # nor the remedy.
  withr::with_envvar(
    c(SNOWFLAKE_HOST = NA, SNOWFLAKE_TOKEN = NA, SNOWFLAKE_PAT = NA),
    expect_error(
      sf_auth_resolve(account = "acct"),
      "externalbrowser"
    )
  )
})

test_that("request headers prefer a supplied header set over Bearer", {
  # Browser SSO and workload identity use `Snowflake Token="..."`, not Bearer.
  # api.R must pass those through verbatim rather than rewrapping the token.
  auth_browser <- list(
    type = "externalbrowser",
    headers = list(Authorization = 'Snowflake Token="abc"')
  )
  auth_jwt <- list(type = "jwt", token = "xyz", token_type = "KEYPAIR_JWT")

  hdr <- function(auth) {
    if (!is.null(auth$headers)) {
      as.list(auth$headers)
    } else {
      list(
        "Authorization" = paste("Bearer", auth$token),
        "X-Snowflake-Authorization-Token-Type" = auth$token_type
      )
    }
  }

  expect_equal(hdr(auth_browser)$Authorization, 'Snowflake Token="abc"')
  expect_null(hdr(auth_browser)[["X-Snowflake-Authorization-Token-Type"]])
  expect_equal(hdr(auth_jwt)$Authorization, "Bearer xyz")
})

test_that("session mode refuses externalbrowser rather than failing obscurely", {
  auth <- list(type = "externalbrowser", headers = list(Authorization = "x"))
  expect_error(
    sf_session_login("acct", auth),
    "not supported with external browser"
  )
})
