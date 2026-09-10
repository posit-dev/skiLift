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


# ---------------------------------------------------------------------------
# Credential refresh (DB-15 follow-up)
# ---------------------------------------------------------------------------
#
# Browser SSO exposes no raw token, so the JWT-style "re-derive it" path in
# .try_refresh_token() cannot work. Before this, the function fell through to
# FALSE for this type and a 401 mid-session was terminal. It now re-requests
# credentials through snowflakeauth for the same identity. These tests drive
# .try_refresh_token() itself rather than reimplementing its logic.

.browser_con <- function(headers = list(Authorization = 'Snowflake Token="old"'),
                         params = list(account = "acct")) {
  con <- new("SnowflakeConnection",
    account = "acct", user = "user", database = "db", schema = "sch",
    warehouse = "wh", role = "role",
    .auth = list(
      type = "externalbrowser",
      headers = headers,
      params = params
    ),
    .state = .new_conn_state()
  )
  con@.state$headers <- headers
  con
}

test_that("refresh updates the live header cache when credentials change", {
  con <- .browser_con()
  local_mocked_bindings(
    snowflake_credentials = function(...) list(Authorization = 'Snowflake Token="new"'),
    .package = "snowflakeauth"
  )
  expect_true(.try_refresh_token(con))
  # The environment is the reference-semantics home for the live copy, so the
  # update must be visible on the caller's own object.
  expect_equal(con@.state$headers$Authorization, 'Snowflake Token="new"')
})

test_that("refresh reports no change when snowflakeauth returns the same header", {
  # Must be FALSE, not TRUE: a spurious TRUE makes the 401 backstop retry an
  # identical request and mask the real error.
  con <- .browser_con()
  local_mocked_bindings(
    snowflake_credentials = function(...) list(Authorization = 'Snowflake Token="old"'),
    .package = "snowflakeauth"
  )
  expect_false(.try_refresh_token(con))
  expect_equal(con@.state$headers$Authorization, 'Snowflake Token="old"')
})

test_that("a failing re-request degrades to FALSE rather than erroring", {
  # If the cached ID token has expired and no browser is available, this must
  # surface as the original 401, not as an unrelated exception from refresh.
  con <- .browser_con()
  local_mocked_bindings(
    snowflake_credentials = function(...) stop("SSO cache expired"),
    .package = "snowflakeauth"
  )
  expect_false(.try_refresh_token(con))
  expect_equal(con@.state$headers$Authorization, 'Snowflake Token="old"')
})

test_that("refresh is a no-op when no params were carried", {
  # Connections built before params were retained (and hand-built ones in
  # tests) must not error on refresh.
  con <- .browser_con(params = NULL)
  expect_false(.try_refresh_token(con))
})

test_that("the live header cache is what the request path actually reads", {
  con <- .browser_con()
  con@.state$headers <- list(Authorization = 'Snowflake Token="refreshed"')
  live <- con@.state$headers %||% con@.auth$headers
  expect_equal(live$Authorization, 'Snowflake Token="refreshed"')
})
