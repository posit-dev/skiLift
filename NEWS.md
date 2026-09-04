# skiLift 0.2.1

## Documentation / Workspace

* **`inst/notebooks/skilift_config.yaml`** -- commented `mirrors` and
  `auth_secret` template aligned with skiPatrol / sfnb-multilang for
  Workspace Notebook Secrets (Artifactory bootstrap).

# skiLift 0.2.0

## New Features

* **Programmatic Access Token (PAT) authentication** -- set `SNOWFLAKE_PAT` or
  pass a token directly via `dbConnect()`. PATs now use the correct
  `PROGRAMMATIC_ACCESS_TOKEN` header.

* **Arrow interface** -- `dbGetQueryArrow()`, `dbSendQueryArrow()`,
  `dbFetchArrow()`, and `dbFetchArrowChunk()` return results as `nanoarrow`
  array streams for compatibility with DBI Arrow workflows. Note: the
  Snowflake SQL API v2 returns JSON, so data is converted to Arrow on the
  client side. This provides interface compatibility but not a performance
  advantage over `dbGetQuery()`. Native server-side Arrow transport is
  planned for a future release.

* **dbplyr backend** -- `tbl()`, `filter()`, `select()`, and other dplyr
verbs  are translated to Snowflake SQL and executed lazily. Inherits
  Snowflake-specific translations from `dbplyr::simulate_snowflake()`.

* **Improved bulk upload** -- `dbWriteTable()` and `dbAppendTable()` now
  generate named-column INSERT statements, use a configurable batch size
  (`options(skiLift.insert_batch_size = N)`), and display a `cli` progress
  bar for large uploads.

* **`dbListObjects()`** -- hierarchical browsing of databases, schemas, and
  tables for RStudio/Positron Connections Pane integration.

* **`dbUnquoteIdentifier()`** -- parses quoted multi-part identifiers
  (e.g., `"db"."schema"."table"`) into `Id` objects.

* **RStudio/Positron Connections Pane hooks** -- connections appear in
  the Connections Pane with object browsing and column preview.

* **Getting-started vignette** covering authentication, queries, Arrow,
  and dbplyr workflows.

## Bug Fixes

* Identifier case is now preserved in `dbCreateTable()` -- column names
  are no longer uppercased, aligning with DBI round-trip semantics.

* S4 mutability fix: `SnowflakeResult` state tracking uses reference
  semantics (environment slot) to avoid R's copy-on-modify behaviour.

* 401 token refresh for JWT authentication is handled transparently.

# skiLift 0.1.0

* Initial release with full DBI compliance via Snowflake SQL API v2.
* JWT key-pair authentication, session-token (Workspace) auth.
* Core DBI methods: `dbConnect`, `dbGetQuery`, `dbExecute`, `dbSendQuery`,
  `dbFetch`, `dbBind`, `dbCreateTable`, `dbWriteTable`, `dbAppendTable`,
  `dbReadTable`, `dbRemoveTable`, `dbListTables`, `dbExistsTable`,
  `dbListFields`, `dbQuoteIdentifier`, `dbQuoteString`, `dbQuoteLiteral`,
  `dbBegin`, `dbCommit`, `dbRollback`, `dbWithTransaction`.
* Snowflake Workspace Notebook support (auto-detects session token).
* `connections.toml` configuration file support.
* 64 unit tests.
