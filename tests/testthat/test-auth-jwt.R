# Tests for JWT account-identifier reduction
# =============================================================================
# sf_generate_jwt() itself is mocked everywhere else (test-token-refresh.R),
# so this transformation had no coverage before this test -- it was only
# caught by a live 401 against an account with a region/cloud suffix.

test_that("account with region/cloud suffix is truncated to the account segment", {
  expect_equal(.jwt_account_identifier("ij38992.eu-west-2.aws"), "IJ38992")
  expect_equal(.jwt_account_identifier("xy12345.us-east-2.azure"), "XY12345")
})

test_that("plain account identifier is unaffected", {
  expect_equal(.jwt_account_identifier("myaccount"), "MYACCOUNT")
})

test_that("org-based account identifier (hyphen, no dot) is unaffected", {
  expect_equal(.jwt_account_identifier("myorg-myaccount"), "MYORG-MYACCOUNT")
})

test_that(".global accounts are truncated at the hyphen, not the dot", {
  expect_equal(.jwt_account_identifier("myorg-myaccount.global"), "MYORG")
})

test_that("account identifier is uppercased", {
  expect_equal(.jwt_account_identifier("lowercase.us-east-1.aws"), "LOWERCASE")
})
