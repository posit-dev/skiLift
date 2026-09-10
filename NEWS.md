# skiLift (development version)

## Package rename

* **This package was `RSnowflake`; it is now `skiLift`.** Snowflake's legal
  position required "snow" out of the *package name*; references to Snowflake
  in code and documentation are unaffected and remain throughout. Replace
  `library(RSnowflake)` with `library(skiLift)`, and any `RSnowflake.*`
  options with `skiLift.*` -- for example
  `options(skiLift.use_session = TRUE)`.

* **`Snowflake()`, `SnowflakeConnection`, `SnowflakeDriver` and
  `SnowflakeResult` keep their names.** DBI convention names the database, not
  the package -- compare `RPostgres::Postgres()` -- so no connection code
  needs to change beyond the package name itself.

## New Features

* **External browser single sign-on** -- pass
  `authenticator = "externalbrowser"` to `dbConnect()`. The flow is delegated
  to `snowflakeauth`: a localhost redirect listener, Snowflake's proof-key
  exchange, and caching of the session token and keyring-backed ID token so
  the browser is not reopened on every connection. Requires the
  `snowflakeauth` and `httpuv` packages and an interactive session; scripts
  and pipelines should use key-pair JWT or a PAT.

  Not available where the credential cannot be used or forwarded, each of
  which now reports why rather than failing obscurely: inside Workspace
  Notebooks and SPCS, where the container's session token is the only valid
  credential; in session mode (`options(skiLift.use_session = TRUE)`); and
  for Snowpark bulk writes.

  An expired browser session is renewed in place on a 401 rather than ending
  the connection.

## Bug Fixes

* **Key-pair JWT authentication now works for accounts with a region or cloud
  suffix.** For identifiers of the form `<account>.<region>.<cloud>`,
  `sf_generate_jwt()` converted every dot to a hyphen -- producing
  `IJ38992-EU-WEST-2-AWS` where Snowflake's JWT algorithm requires the
  account segment alone, `IJ38992`. The claim was well-formed but wrong, so
  Snowflake correctly rejected it with HTTP 401 / 390144. The suffixed form is
  the common shape for real accounts, so key-pair JWT was very likely broken
  for most real-world use.

* **Connections now succeed from Posit Workbench and the Posit Team Native
  App.** `dbConnect()` reads the short-lived OAuth `token` from a
  `connections.toml` profile and maps `authenticator = "oauth"` to the correct
  header; previously both environments failed with "No credentials found".

* **Token refresh now reaches the connection at all.** The refreshed token was
  written to a value-copied S4 slot, so it never became visible to the caller
  and neither the 401-retry backstop nor JWT refresh had ever taken effect.
  The live token is now held with reference semantics.

* **A rotated OAuth token is re-read correctly.** Refresh previously re-read
  the `connections.toml` file raw and handed the wire the file's own TOML
  syntax as the token, failing with HTTP 400 / 390146 "Bearer token is
  missing". It now re-parses the document and re-selects the same profile.

* **Workspace traffic is no longer misrouted.** A `connections.toml` OAuth
  token is minted for the public endpoint, and is no longer sent to the
  internal SPCS gateway when `SNOWFLAKE_HOST` happens to be set alongside a
  local profile.

* `DESCRIPTION` declared `Apache License (>= 2)` while both `LICENSE` and
  `NOTICE` grant Version 2.0 specifically. Now consistently 2.0.

## Documentation and packaging

* The zero-argument connect path is documented: with no arguments, a
  connection authenticates as the Snowflake user who launched the session --
  not a shared service account -- using that user's default role, warehouse
  and database unless overridden.

* Development notebooks and performance harnesses are no longer installed as
  example content; they now live in `tests/notebooks/`.

* Added a top-level `NOTICE` recording Snowflake Labs as the origin of the
  work, Posit Software, PBC as copyright holder and funder, and Hex Field Ltd
  as maintainer, per Apache-2.0 section 4(b).

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
