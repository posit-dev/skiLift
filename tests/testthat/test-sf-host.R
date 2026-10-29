# Host names
# =============================================================================
# Underscores are not valid in host names. Snowflake accepts an account written
# with each underscore as a hyphen, for use "when a URL or feature doesn't
# accept underscores". Only the host changes.

test_that("an account with underscores gets hyphens in the host", {
  expect_equal(sf_host("acme-marketing_test"), "https://acme-marketing-test.snowflakecomputing.com")
  expect_equal(sf_host("my_org-my_acct"), "https://my-org-my-acct.snowflakecomputing.com")
})

test_that("an account with no underscore is unchanged", {
  expect_equal(sf_host("myorg-myacct"), "https://myorg-myacct.snowflakecomputing.com")
  expect_equal(sf_host("ij38992.eu-west-2.aws"), "https://ij38992.eu-west-2.aws.snowflakecomputing.com")
})

test_that("the Workspace host is used as given, whatever the account is called", {
  auth <- list(type = "oauth", host_eligible = TRUE)
  withr::with_envvar(c(SNOWFLAKE_HOST = "acct_x.snowflake.internal"), {
    expect_equal(sf_host("acct_x", auth), "https://acct_x.snowflake.internal")
  })
})

test_that("dbGetInfo and the Connections Pane build the host the same way", {
  con <- new("SnowflakeConnection", account = "acme_test", user = "u", database = "", schema = "",
             warehouse = "", role = "", .auth = list(), .state = .new_conn_state())
  expect_equal(.sf_hostname("acme_test"), "acme-test.snowflakecomputing.com")
  expect_equal(DBI::dbGetInfo(con)$host, "acme-test.snowflakecomputing.com")
})

test_that("the account name is not changed anywhere else: the JWT claim keeps its underscore", {
  skip_if_not_installed("openssl")
  key <- tempfile(fileext = ".p8")
  openssl::write_pem(openssl::rsa_keygen(2048), key)
  auth <- withr::with_envvar(c(SNOWFLAKE_HOST = NA, SNOWFLAKE_TOKEN = NA, SNOWFLAKE_PAT = NA),
    sf_auth_resolve(account = "acme_test", user = "u", private_key_path = key))
  token <- auth$token   # on main the key-pair JWT is auth$token (the branch uses snowflakeauth headers)
  payload <- chartr("-_", "+/", strsplit(token, ".", fixed = TRUE)[[1L]][[2L]])
  payload <- paste0(payload, strrep("=", (4 - nchar(payload) %% 4) %% 4))
  claims <- jsonlite::fromJSON(rawToChar(openssl::base64_decode(payload)))
  expect_match(claims$sub, "ACME_TEST")
})
