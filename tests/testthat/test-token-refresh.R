# Tests for the auth-token refresh cache (.state) and its propagation
# =============================================================================
# Regression coverage for the defect fixed 4 Sep 2026: .try_refresh_token()
# used to write into con@.auth, a value-copied S4 list slot, so the refresh
# was discarded on return and the caller's retry resent the stale token.
# See internal/posit_native_app_support/p1-02-design-review.md #1.

.mock_token_con <- function(type = "oauth", token = "old-token",
                             token_file = NULL, token_type = "OAUTH",
                             token_source = NULL, token_profile = NULL) {
  auth <- list(type = type, token = token, token_type = token_type)
  if (!is.null(token_file)) auth$token_file <- token_file
  if (!is.null(token_source)) auth$token_source <- token_source
  if (!is.null(token_profile)) auth$token_profile <- token_profile

  con <- new("SnowflakeConnection",
    account = "test", user = "user", database = "db", schema = "sch",
    warehouse = "wh", role = "role",
    .auth = auth,
    .state = .new_conn_state()
  )
  con@.state$token <- token
  con
}

# ---------------------------------------------------------------------------
# .try_refresh_token() -- propagation via .state
# ---------------------------------------------------------------------------

test_that("refreshing an oauth/token connection updates .state, not just a local copy", {
  con <- .mock_token_con(type = "oauth", token = "old-token")
  con_alias <- con  # a second reference, as a retry path would hold

  withr::with_envvar(c(SNOWFLAKE_TOKEN = "new-token"), {
    refreshed <- .try_refresh_token(con)
    expect_true(refreshed)
  })

  # The mutation must be visible through *any* reference to the same
  # connection -- proof that it landed in the environment slot, not one
  # that only the function's local `con` would have seen.
  expect_equal(con@.state$token, "new-token")
  expect_equal(con_alias@.state$token, "new-token")
  expect_equal(con@.auth$token, "old-token")  # .auth is untouched, by design
})

test_that("refresh reports FALSE when the workspace token has not changed", {
  con <- .mock_token_con(type = "token", token = "same-token")
  withr::with_envvar(c(SNOWFLAKE_TOKEN = "same-token"), {
    expect_false(.try_refresh_token(con))
  })
  expect_equal(con@.state$token, "same-token")
})

test_that("refreshing a jwt connection regenerates and stores the token in .state", {
  con <- .mock_token_con(type = "jwt", token = "old-jwt")
  con@.auth$account <- "acct"
  con@.auth$user <- "user"
  con@.auth$private_key_path <- "irrelevant.pem"

  mockr::with_mock(
    sf_generate_jwt = function(account, user, private_key_path) "new-jwt",
    {
      expect_true(.try_refresh_token(con))
    }
  )
  expect_equal(con@.state$token, "new-jwt")
  expect_true(is.numeric(con@.state$token_mtime))
})

# ---------------------------------------------------------------------------
# .sf_api_request_raw() reads the live token, not the connect-time one
# ---------------------------------------------------------------------------

test_that("outgoing requests use the refreshed token from .state, not @.auth", {
  con <- .mock_token_con(type = "oauth", token = "stale-in-auth")
  con@.state$token <- "fresh-in-state"

  captured <- NULL
  mock_fn <- function(req) {
    captured <<- httr2::req_get_headers(req, redacted = "reveal")$Authorization
    httr2::response(status_code = 200, body = charToRaw("{}"))
  }

  httr2::local_mocked_responses(mock_fn)
  .sf_api_request_raw(con, "POST", "https://example.snowflakecomputing.com/x")

  expect_equal(captured, "Bearer fresh-in-state")
})

# ---------------------------------------------------------------------------
# .refresh_token_if_stale() -- mtime-gated re-read ahead of the 401 backstop
# ---------------------------------------------------------------------------

test_that(".refresh_token_if_stale is a no-op for tokens with no backing file", {
  con <- .mock_token_con(type = "oauth", token = "t1")  # no token_file
  .refresh_token_if_stale(con)
  expect_equal(con@.state$token, "t1")
  expect_null(con@.state$token_mtime)
})

test_that(".refresh_token_if_stale is a no-op for non-file-rotating auth types", {
  con <- .mock_token_con(type = "jwt", token = "t1")
  .refresh_token_if_stale(con)
  expect_equal(con@.state$token, "t1")
})

test_that(".refresh_token_if_stale re-reads once, then skips while mtime is unchanged", {
  tmp <- tempfile()
  writeLines("token-v1", tmp)
  con <- .mock_token_con(type = "oauth", token = "connect-time-token", token_file = tmp)

  .refresh_token_if_stale(con)
  expect_equal(con@.state$token, "token-v1")
  mtime_after_first <- con@.state$token_mtime
  expect_true(is.numeric(mtime_after_first))

  # Rewrite the file's contents but pin its mtime back -- simulates calling
  # again inside the same rotation window, where the stat should short-circuit.
  writeLines("token-v2-should-not-be-picked-up", tmp)
  Sys.setFileTime(tmp, as.POSIXct(mtime_after_first, origin = "1970-01-01"))

  .refresh_token_if_stale(con)
  expect_equal(con@.state$token, "token-v1")

  unlink(tmp)
})

test_that(".refresh_token_if_stale re-reads once the file's mtime advances", {
  tmp <- tempfile()
  writeLines("token-v1", tmp)
  con <- .mock_token_con(type = "oauth", token = "connect-time-token", token_file = tmp)

  .refresh_token_if_stale(con)
  expect_equal(con@.state$token, "token-v1")

  Sys.setFileTime(tmp, Sys.time() + 5)  # force a later mtime regardless of fs resolution
  writeLines("token-v2", tmp)
  Sys.setFileTime(tmp, Sys.time() + 5)

  .refresh_token_if_stale(con)
  expect_equal(con@.state$token, "token-v2")

  unlink(tmp)
})

test_that(".file_mtime handles NULL and missing paths", {
  expect_true(is.na(.file_mtime(NULL)))
  expect_true(is.na(.file_mtime(tempfile())))  # does not exist

  tmp <- tempfile()
  writeLines("x", tmp)
  expect_true(is.numeric(.file_mtime(tmp)) && !is.na(.file_mtime(tmp)))
  unlink(tmp)
})

# ---------------------------------------------------------------------------
# sf_auth_resolve() reports the source file for rotation-eligible tokens
# ---------------------------------------------------------------------------

test_that("sf_auth_resolve reports token_file for the SPCS token-file path", {
  tmp <- tempfile()
  writeLines("filetoken", tmp)

  withr::with_envvar(c(SNOWFLAKE_HOST = "internal.example", SNOWFLAKE_TOKEN = ""), {
    mockr::with_mock(
      .read_workspace_token = function() list(token = "filetoken", file = tmp),
      {
        result <- sf_auth_resolve(account = "test", user = "user")
        expect_equal(result$type, "oauth")
        expect_equal(result$token_file, tmp)
      }
    )
  })
  unlink(tmp)
})

test_that("sf_auth_resolve reports no token_file for an env-var-sourced token", {
  withr::with_envvar(c(SNOWFLAKE_HOST = "", SNOWFLAKE_TOKEN = "envtoken", SNOWFLAKE_PAT = ""), {
    result <- sf_auth_resolve(account = "test", user = "user")
    expect_equal(result$type, "token")
    expect_null(result$token_file)
  })
})

# ---------------------------------------------------------------------------
# Regression: refreshing a toml-sourced token must re-parse the TOML, not
# slurp the whole file as if it were a bare token file.
#
# Found live in the Native App, 4 Sep 2026: .refresh_token_if_stale() and
# .try_refresh_token() both read a rotated token_file with plain
# readLines(), which is right for Workspace's bare /snowflake/session/token
# but for a connections.toml OAuth profile hands the wire a "token" that is
# literally the file's [section]/account/authenticator/token lines
# concatenated together -- observed as a live 400/390146 "Bearer token is
# missing" after the first (correct) query on a fresh connection. Fixed by
# tagging the auth object with token_source = "toml" so refresh re-parses
# via sf_read_connections_toml() and re-selects token_profile, instead of
# reading token_file raw. See p1-02-design-review.md addendum, 4 Sep.
# ---------------------------------------------------------------------------

.write_workbench_toml <- function(dir, token) {
  writeLines(c(
    "[workbench]",
    'account = "wbacct"',
    'authenticator = "oauth"',
    sprintf('token = "%s"', token)
  ), file.path(dir, "connections.toml"))
}

test_that(".read_token_from_source re-parses a toml source instead of reading it raw", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir, showWarnings = FALSE)
  .write_workbench_toml(tmp_dir, "wb-token-v2")

  withr::with_envvar(c(SNOWFLAKE_HOME = tmp_dir), {
    auth <- list(
      token_source = "toml",
      token_file = file.path(tmp_dir, "connections.toml"),
      token_profile = "workbench"
    )
    token <- .read_token_from_source(auth)
    expect_equal(token, "wb-token-v2")
    # The defect this guards against: reading the file raw would produce
    # something containing the TOML's own syntax, not a bare token.
    expect_false(grepl("[[]workbench[]]|authenticator", token, fixed = FALSE))
  })
  unlink(tmp_dir, recursive = TRUE)
})

test_that(".refresh_token_if_stale re-parses a rotated toml, not the raw file", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir, showWarnings = FALSE)
  toml_file <- file.path(tmp_dir, "connections.toml")
  .write_workbench_toml(tmp_dir, "wb-token-v1")

  con <- .mock_token_con(
    type = "oauth", token = "wb-token-v1",
    token_file = toml_file, token_source = "toml", token_profile = "workbench"
  )
  # Simulate having connected earlier: cached_mtime predates the file.
  con@.state$token_mtime <- .file_mtime(toml_file) - 10

  withr::with_envvar(c(SNOWFLAKE_HOME = tmp_dir), {
    .refresh_token_if_stale(con)
  })

  expect_equal(con@.state$token, "wb-token-v1")
  expect_false(grepl("workbench", con@.state$token, fixed = TRUE))
})

test_that(".refresh_token_if_stale picks up a genuinely rotated toml token", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir, showWarnings = FALSE)
  toml_file <- file.path(tmp_dir, "connections.toml")
  .write_workbench_toml(tmp_dir, "wb-token-old")

  con <- .mock_token_con(
    type = "oauth", token = "wb-token-old",
    token_file = toml_file, token_source = "toml", token_profile = "workbench"
  )
  con@.state$token_mtime <- .file_mtime(toml_file)

  Sys.setFileTime(toml_file, Sys.time() + 5)
  .write_workbench_toml(tmp_dir, "wb-token-new")
  Sys.setFileTime(toml_file, Sys.time() + 5)

  withr::with_envvar(c(SNOWFLAKE_HOME = tmp_dir), {
    .refresh_token_if_stale(con)
  })

  expect_equal(con@.state$token, "wb-token-new")
  unlink(tmp_dir, recursive = TRUE)
})

test_that(".try_refresh_token's 401 backstop also re-parses a toml source", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir, showWarnings = FALSE)
  toml_file <- file.path(tmp_dir, "connections.toml")
  .write_workbench_toml(tmp_dir, "wb-token-fresh")

  con <- .mock_token_con(
    type = "oauth", token = "wb-token-stale",
    token_file = toml_file, token_source = "toml", token_profile = "workbench"
  )

  withr::with_envvar(c(SNOWFLAKE_HOME = tmp_dir), {
    refreshed <- .try_refresh_token(con)
  })

  expect_true(refreshed)
  expect_equal(con@.state$token, "wb-token-fresh")
  unlink(tmp_dir, recursive = TRUE)
})
