# Tests for connections.toml OAuth profiles (P1-02, changes 1-2)
# =============================================================================
# Covers the diagnosed Native App defect: neither package read `token` from
# a connections.toml profile with authenticator = "oauth" (Posit Workbench /
# the Native App writes exactly this shape). See
# internal/posit_native_app_support/native-app-probe-findings.md and
# internal/posit_native_app_support/p1-02-design-review.md.

.write_oauth_profile <- function(name = "workbench", token = "wb-token-123",
                                  account = "wbacct") {
  tmp_dir <- tempfile()
  dir.create(tmp_dir, showWarnings = FALSE)
  writeLines(c(
    sprintf("[%s]", name),
    sprintf('account = "%s"', account),
    'authenticator = "oauth"',
    sprintf('token = "%s"', token)
  ), file.path(tmp_dir, "connections.toml"))
  tmp_dir
}

# ---------------------------------------------------------------------------
# sf_read_connections_toml() reports its source
# ---------------------------------------------------------------------------

test_that("sf_read_connections_toml attaches toml_file and toml_name", {
  tmp_dir <- .write_oauth_profile()
  withr::with_envvar(c(SNOWFLAKE_HOME = tmp_dir), {
    profile <- sf_read_connections_toml("workbench")
    expect_equal(profile$token, "wb-token-123")
    expect_equal(attr(profile, "toml_name"), "workbench")
    expect_equal(attr(profile, "toml_file"), file.path(tmp_dir, "connections.toml"))
  })
  unlink(tmp_dir, recursive = TRUE)
})

# ---------------------------------------------------------------------------
# sf_auth_resolve() Priority 6 -- toml OAuth profile
# ---------------------------------------------------------------------------

test_that("sf_auth_resolve maps a profile token + authenticator=oauth to type=oauth", {
  withr::with_envvar(c(SNOWFLAKE_HOST = "", SNOWFLAKE_TOKEN = "", SNOWFLAKE_PAT = ""), {
    result <- sf_auth_resolve(
      account = "wbacct", user = NULL,
      authenticator = "oauth",
      profile_token = "wb-token-123",
      profile_token_file = "/tmp/connections.toml"
    )
    expect_equal(result$type, "oauth")
    expect_equal(result$token, "wb-token-123")
    expect_equal(result$token_type, "OAUTH")
    expect_equal(result$token_file, "/tmp/connections.toml")
    # Must not be flagged eligible for the internal SPCS gateway -- this
    # token is for the public endpoint. See the sf_host() tests below.
    expect_null(result$host_eligible)
  })
})

test_that("a profile token without authenticator=oauth is not treated as OAuth", {
  withr::with_envvar(c(SNOWFLAKE_HOST = "", SNOWFLAKE_TOKEN = "", SNOWFLAKE_PAT = ""), {
    expect_error(
      sf_auth_resolve(
        account = "wbacct", user = NULL,
        authenticator = NULL,
        profile_token = "wb-token-123",
        profile_token_file = "/tmp/connections.toml"
      ),
      "No Snowflake credentials found"
    )
  })
})

test_that("a real credential still wins over a profile token (priority order holds)", {
  withr::with_envvar(c(SNOWFLAKE_HOST = "", SNOWFLAKE_TOKEN = "", SNOWFLAKE_PAT = "envpat"), {
    result <- sf_auth_resolve(
      account = "wbacct", user = NULL,
      authenticator = "oauth",
      profile_token = "wb-token-123",
      profile_token_file = "/tmp/connections.toml"
    )
    expect_equal(result$type, "pat")
    expect_equal(result$token, "envpat")
  })
})

# ---------------------------------------------------------------------------
# sf_host() misroute guard -- host_eligible, not just type == "oauth"
# ---------------------------------------------------------------------------

test_that("sf_host does not route a toml-sourced OAuth token to SNOWFLAKE_HOST", {
  withr::with_envvar(c(SNOWFLAKE_HOST = "internal.example"), {
    auth <- list(type = "oauth", token = "wb-token-123")  # no host_eligible
    expect_equal(sf_host("wbacct", auth), "https://wbacct.snowflakecomputing.com")
  })
})

test_that("sf_host still routes genuine Workspace/SPCS OAuth to SNOWFLAKE_HOST", {
  withr::with_envvar(c(SNOWFLAKE_HOST = "internal.example"), {
    auth <- list(type = "oauth", token = "spcs-token", host_eligible = TRUE)
    expect_equal(sf_host("wbacct", auth), "https://internal.example")
  })
})

# ---------------------------------------------------------------------------
# .resolve_connect_params() -- the dbConnect() merge logic, without the
# network-hitting validation query dbConnect() itself always performs
# (which is why no existing test in this suite calls dbConnect() directly).
# ---------------------------------------------------------------------------

.resolve <- function(account = NULL, user = NULL, token = NULL,
                      private_key_path = NULL, authenticator = NULL,
                      database = "", schema = "", warehouse = "", role = "",
                      name = NULL) {
  .resolve_connect_params(
    account = account, user = user, token = token,
    private_key_path = private_key_path, authenticator = authenticator,
    database = database, schema = schema, warehouse = warehouse,
    role = role, name = name
  )
}

test_that("resolving a Native-App-shaped profile with no account given picks up the token", {
  tmp_dir <- .write_oauth_profile(name = "workbench", token = "wb-token-abc")

  withr::with_envvar(c(SNOWFLAKE_HOME = tmp_dir, SNOWFLAKE_HOST = "",
                        SNOWFLAKE_TOKEN = "", SNOWFLAKE_PAT = ""), {
    resolved <- .resolve(name = "workbench")
    expect_equal(resolved$account, "wbacct")
    expect_equal(resolved$authenticator, "oauth")
    expect_equal(resolved$profile_token, "wb-token-abc")
    expect_equal(resolved$profile_token_file, file.path(tmp_dir, "connections.toml"))
  })
  unlink(tmp_dir, recursive = TRUE)
})

test_that("account = ..., name = ... together still find the profile token", {
  # The hazard flagged in the design review: previously the toml was only
  # read when account was NULL, so pinning account explicitly alongside an
  # explicit profile name silently lost profile$token.
  tmp_dir <- .write_oauth_profile(name = "workbench", token = "wb-token-xyz",
                                   account = "wbacct")

  withr::with_envvar(c(SNOWFLAKE_HOME = tmp_dir, SNOWFLAKE_HOST = "",
                        SNOWFLAKE_TOKEN = "", SNOWFLAKE_PAT = ""), {
    resolved <- .resolve(account = "wbacct", name = "workbench")
    expect_equal(resolved$account, "wbacct")
    expect_equal(resolved$authenticator, "oauth")
    expect_equal(resolved$profile_token, "wb-token-xyz")
  })
  unlink(tmp_dir, recursive = TRUE)
})

test_that("account = ... without name does not merge an unrelated default profile", {
  # account given, no name: must NOT eagerly adopt a same-machine default
  # profile's credentials for an account the caller never asked it for.
  tmp_dir <- .write_oauth_profile(name = "default", token = "unrelated-token",
                                   account = "otheracct")

  withr::with_envvar(c(SNOWFLAKE_HOME = tmp_dir, SNOWFLAKE_HOST = "",
                        SNOWFLAKE_TOKEN = "", SNOWFLAKE_PAT = ""), {
    resolved <- .resolve(account = "wbacct", user = "u")
    expect_equal(resolved$account, "wbacct")  # untouched
    expect_null(resolved$profile_token)       # the unrelated profile's token is not adopted
  })
  unlink(tmp_dir, recursive = TRUE)
})

test_that("an explicit token is never displaced by a profile token", {
  tmp_dir <- .write_oauth_profile(name = "workbench", token = "wb-token-abc")

  withr::with_envvar(c(SNOWFLAKE_HOME = tmp_dir, SNOWFLAKE_HOST = "",
                        SNOWFLAKE_TOKEN = "", SNOWFLAKE_PAT = ""), {
    resolved <- .resolve(token = "explicit-pat", name = "workbench")
    expect_null(resolved$profile_token)
  })
  unlink(tmp_dir, recursive = TRUE)
})

# ---------------------------------------------------------------------------
# End to end, through sf_auth_resolve() -- the pieces above composed
# ---------------------------------------------------------------------------

test_that("the full resolution -- toml merge then auth resolve -- reaches type=oauth", {
  tmp_dir <- .write_oauth_profile(name = "workbench", token = "wb-token-abc")

  withr::with_envvar(c(SNOWFLAKE_HOME = tmp_dir, SNOWFLAKE_HOST = "",
                        SNOWFLAKE_TOKEN = "", SNOWFLAKE_PAT = ""), {
    resolved <- .resolve(name = "workbench")
    auth <- sf_auth_resolve(
      account = resolved$account, user = resolved$user,
      authenticator = resolved$authenticator,
      private_key_path = resolved$private_key_path,
      profile_token = resolved$profile_token,
      profile_token_file = resolved$profile_token_file
    )
    expect_equal(auth$type, "oauth")
    expect_equal(auth$token, "wb-token-abc")
    expect_equal(auth$token_type, "OAUTH")
    expect_null(auth$host_eligible)
  })
  unlink(tmp_dir, recursive = TRUE)
})
