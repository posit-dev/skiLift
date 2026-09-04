# DBItest compliance suite - requires live Snowflake connection.
# Skipped unless SKILIFT_DBITEST=true is set.

skip_if(
  !nzchar(Sys.getenv("SKILIFT_DBITEST", "")),
  "DBItest skipped (set SKILIFT_DBITEST=true to enable)"
)

skip_if_not_installed("DBItest")

library(DBItest)

old_case <- getOption("skiLift.identifier_case")
options(skiLift.identifier_case = "preserve")
withr::defer(options(skiLift.identifier_case = old_case))

# Snowflake-specific tweaks
sf_tweaks <- tweaks(
  constructor_name = "Snowflake",
  strict_identifier = TRUE,
  omit_blob_tests = TRUE,
  temporary_tables = FALSE,
  list_temporary_tables = FALSE,
  placeholder_pattern = "\\?",
  logical_return = function(x) as.logical(x),
  date_cast = function(x) paste0("'", x, "'::DATE"),
  time_cast = function(x) paste0("'", x, "'::TIME"),
  timestamp_cast = function(x) paste0("'", x, "'::TIMESTAMP_NTZ")
)

# Tests that are expected to fail due to SQL API v2 limitations
# or Snowflake-specific behavior differences
sf_skip <- c(
  # Transactions not supported via SQL API v2
  "begin_write_commit",
  "begin_write_rollback",
  "begin_write_disconnect",
  "with_transaction_success",
  "with_transaction_failure",
  "with_transaction_name",
  "begin_commit",
  "begin_rollback",
  "begin_begin",

  # Parameterized queries: SQL API v2 bindings have limited type support

  "data_type_connection",

  # Snowflake identifier handling differs from standard SQL
  "quote_identifier_not_plain",

  # BLOB columns not supported in this JSON-based path
  "data_raw",
  "data_raw_null_below",
  "data_raw_null_above",

  # Temporary tables not supported
  "create_temporary_table",
  "list_tables_temporary",

  # Snowflake may return different precision for certain operations
  "data_timestamp",
  "data_timestamp_current",
  "data_timestamp_null_below",
  "data_timestamp_null_above",
  "data_date_current",
  "data_time_current",

  # Roundtrip precision
  "roundtrip_timestamp",
  "roundtrip_date",
  "roundtrip_time",
  "roundtrip_raw",
  "roundtrip_64_bit",

  # Stress tests can be slow / flaky over REST
  "stress_load_unload_.*",
  "stress_simultaneous_connections",

  # These expect specific column-name behavior
  "column_info",
  "get_info_result",
  "rows_affected_query",
  "rows_affected_statement",

  NULL
)

ctx <- make_context(
  Snowflake(),
  connect_args = list(name = Sys.getenv("SKILIFT_PROFILE", "default")),
  tweaks = sf_tweaks,
  default_skip = sf_skip
)

test_getting_started(skip = sf_skip)
test_driver(skip = sf_skip)
test_connection(skip = sf_skip)
test_result(skip = sf_skip)
test_sql(skip = sf_skip)
test_meta(skip = sf_skip)
test_compliance(skip = sf_skip)
