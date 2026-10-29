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

## Reliability

* **A statement that took more than 45 seconds returned no rows, or reported
  success before it had finished; it is fixed. If you ran long queries or long
  `INSERT`, `CREATE TABLE AS` or `CALL` statements with an earlier version
  (including `RSnowflake`), check the results.** Snowflake answers a statement
  that runs longer than 45 seconds with a handle, not a result. skiLift took that
  answer for the result, so a slow `dbGetQuery()` returned a data frame with no
  rows and no columns, and a slow `dbExecute()` returned at once with the
  statement still running (and possibly about to fail). skiLift now waits,
  checking again every 0.5 to 5 seconds, until the statement has finished. If it
  has not finished after `options(skiLift.timeout)` seconds (600 by default) it
  is cancelled in Snowflake and an error of class `skilift_error_timeout` names
  the statement. Pressing Esc while waiting cancels the statement too, so it does
  not keep running and using credits. Fetching a large result in pieces waits
  for any piece Snowflake is still preparing.

* **A retried statement can no longer run twice.** Every statement now carries a
  request id. When a request is sent again after a temporary failure (429, 500,
  502, 503, 504, a dropped connection or a timeout), the same id goes with
  `retry=true`, and Snowflake does not run the statement again if it already
  ran. Before, a 503 that arrived after an `INSERT` had run led to a blind
  resend, which could write the same rows twice. Failures with no response were
  not retried at all, and the request timeout applied to every attempt, so three
  attempts could take more than 30 minutes. Now `options(skiLift.timeout)` is
  one limit for the whole statement, however many attempts it takes.

* **A request that hangs no longer holds up a statement for ten minutes.** Twice
  in testing a request got no answer at all ("0 bytes received") and waited out
  the whole 600 seconds, and once 16 minutes. Snowflake answers a statement
  within about 45 seconds, so one request is now given
  `options(skiLift.request_timeout)` seconds (90 by default), then abandoned and
  sent again. The resend carries the same request id, so the statement still runs
  only once. Downloading a large result is not limited by it.

* **Errors can be caught by class,** and carry the facts as data: `status`,
  `code`, `sql_state`, `statement_handle`, `request_id` and `attempts`. The
  classes are `skilift_error_sql` (422), `skilift_error_auth` (401),
  `skilift_error_forbidden` (403), `skilift_error_timeout`,
  `skilift_error_retry` (temporary failures that outlasted the retries),
  `skilift_error_write` and `skilift_error_config`; see `?"skilift-errors"`.
  Message text is unchanged, and now ends with a hint where there is one: for a
  key-pair 401, to compare `RSA_PUBLIC_KEY_FP` with the key and check the clock;
  for a 403, the network policy; for "does not exist or not authorized", a
  missing privilege and not a sign-in problem. A server message that contained
  `{` or `}` used to make the error itself fail; it is now shown as text.

* **Writes are limited by size as well as by rows, and a failed write says how
  far it got.** A batch now ends at `skiLift.insert_batch_size` rows or
  `skiLift.insert_batch_bytes` bytes of SQL (about 1 MB), whichever comes first.
  The row limit alone let one request grow to many megabytes with wide rows or
  long text. A single row too large to send is an error, before anything is sent
  for it. Each batch still commits on its own, so **a failed write is not rolled
  back**; but the `skilift_error_write` error now reports `rows_committed`, the
  failed batch, and an `outcome` that says whether the failed batch is in the
  table: `"failed"` (it ran and failed), `"not_executed"`, or `"unknown"` (it may
  have been committed; check the request id before writing again).

* **Transactions are not supported, and now say so.** `dbBegin()`, `dbCommit()`,
  `dbRollback()` and `dbWithTransaction()` raise an error of class
  `skilift_error_config`. **They did not work before, and the failure was
  silent:** with `options(skiLift.use_session = TRUE)`, `dbBegin()` sent only
  `BEGIN`, `COMMIT` and `ROLLBACK` through the session, while every statement in
  between was sent through the SQL API and committed at once. A rolled-back
  transaction left its rows in the table. To make changes succeed or fail
  together, send them as one Snowflake Scripting block, for example
  `dbExecute(con, "EXECUTE IMMEDIATE $$ BEGIN BEGIN TRANSACTION; ...; COMMIT; END $$")`;
  see `?dbBegin`.

* **Session mode (`options(skiLift.use_session = TRUE)`) could not run a
  query; it can now.** Its requests had no request id, which Snowflake rejects,
  and a malformed `Authorization` header. A session that was refused with 401
  retried without limit until R ran out of stack; it now renews once and then
  reports the error.

* **`dbAppendTable()` and `dbCreateTable()` with a `DBI::Id()` table name
  failed; they work now.** Without a method for `Id` they fell through to DBI's
  generic versions, which use multi-row parameter binding, and that skiLift does
  not support. `dbAppendTable(con, DBI::Id(schema = "S", table = "T"), df)` is
  the standard DBI way to name a table in a schema, and what dplyr's
  `rows_append()` relies on.

* **Account names with underscores** now use the hyphen form in the host name
  (`acme_test` connects to `acme-test.snowflakecomputing.com`), as Snowflake
  documents for places that do not accept underscores. This is a precaution: we
  have not seen an underscore host name fail. The account name is unchanged
  everywhere else, including in key-pair sign-in.

## New Features

* **Whole-number columns now have one R type per column, decided by the
  column and not by the values in a result -- and a new `bigint` argument to
  `dbConnect()` controls it.** This changes what some existing queries return.
  Snowflake stores `INTEGER`, `INT`, `BIGINT`, `SMALLINT` and `NUMBER(38,0)`
  identically, so the type a column was created with cannot be recovered; the
  only information available is its precision. Previously the package tried
  to fit each result into an R integer and quietly switched to double if any
  value was too large, so the same column could come back as integer in one
  query and double in the next, and an empty result differed from a non-empty
  one. Now:
  * whole numbers of up to 9 digits are `integer`;
  * larger ones -- which includes every plain `INTEGER` column and
    `COUNT(*)` -- are `numeric` (a double, exact up to 9,007,199,254,740,992);
  * columns with decimals are `numeric`, as before;
  * `dbConnect(..., bigint = "integer64")` (or `"integer"`, or `"character"`)
    returns the larger whole numbers as that type instead. `"integer64"`
    (needs the `bit64` package) keeps every 64-bit value exactly, but many R
    functions do not understand it and some give wrong answers without a
    warning (`lm()`, for example), so use it only if you need values above
    9,007,199,254,740,992. To change the choice for every connection, set
    `options(skiLift.bigint = "integer64")`. A value too large for the chosen
    type is returned as `NA` with a warning, never silently.

  Zero-row results now have the same column classes as results with rows;
  before, `DATE`, `TIME` and timestamp columns came back as plain numbers.

* **`TIMESTAMP_LTZ` and `TIMESTAMP_TZ` values are now shown in Snowflake's
  session time zone, and a new `timezone_out` argument to `dbConnect()` lets
  you choose another.** Previously `TIMESTAMP_LTZ` was labelled with your R
  session's time zone and `TIMESTAMP_TZ` with UTC. The default,
  `"database"`, is the zone Snowflake's own SQL functions use (and its Python
  and ADBC drivers return), so an hour-of-day worked out in R now agrees with
  one worked out in a Snowflake query. Use `"local"` for your R session's
  zone, or any name from `OlsonNames()`, such as `"UTC"`; to change it for
  every connection, set `options(skiLift.timezone_out = "UTC")`. This only
  changes how a time is labelled, never which moment it is. `TIMESTAMP_NTZ`
  has no zone and is labelled `"UTC"`.

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

* **`dbFetch(res, n = k)` silently dropped rows when a result was fetched in
  pieces; it is fixed. If you have processed a result in chunks with an earlier
  version (including `RSnowflake`), check it.** A loop such as
  `while (!dbHasCompleted(res)) dbFetch(res, n = 1000)` returned the first
  1,000 rows and then nothing: the rest of the first block of rows Snowflake
  sent was thrown away, and later blocks were never read. No error or warning
  was given, and `dbHasCompleted()` reported the result complete. For example,
  fetching a 10-row result 3 rows at a time returned rows 1 to 3 only. Fetching
  everything at once (`dbGetQuery()`, `dbFetch(res)`, `collect()`) was not
  affected. Each call now returns exactly the next `n` rows, in order, and reads
  further blocks only when it needs them.

* **Smaller fixes found by running the DBI conformance tests:**
  `dbQuoteString()` of zero strings returned `''` (an empty-string literal)
  instead of nothing, so an empty `IN ()` list could become `IN ('')`;
  `dbHasCompleted()` is now `TRUE` for a statement that has run;
  `dbGetRowsAffected()` is `0` for a query, as DBI specifies, not `-1`.

* **Binary data written with `dbWriteTable()` could be stored as text, and
  `field.types` was silently ignored; both are fixed. If you have written a
  list of raw vectors with an earlier version (including `RSnowflake`), check
  the column.** A plain list of raw vectors, which is how DBI holds binary data,
  was not recognised as binary, so the table got a `TEXT` column containing the
  words `as.raw(c(0xde, 0xad, ...))` instead of the bytes. (A `blob` column
  was written correctly.) `field.types`, for example
  `dbWriteTable(con, "t", df, field.types = c(id = "BIGINT"))`, was ignored, so
  the column got skiLift's own choice of type; it is now applied, and a name
  that is not a column of the data is an error. `row.names = TRUE` also
  dropped the row names without a word; it now gives a warning.

* **Very large, very small and very precise decimals now go in and come back
  exactly.** Found by comparing every value with what Snowflake itself holds.
  * Writing: decimals are always sent with 17 significant digits, which
    identifies any double exactly. Whole numbers below 10^15 are sent as plain
    integers.
  * Reading: R's own text-to-number conversion is not exact for the largest
    and smallest doubles (`1.797693134862316e308` became `Inf`, and `1e-300`
    was out by about 30 units in the last place). skiLift now reads
    `FLOAT` and decimal columns with the JSON parser, which is exact.
  * `DATE` columns now read every date up to 9999-12-31, including dates
    that Snowflake sends as text rather than as a day count.

* **`BINARY` columns are now returned as a `blob` of the bytes,** where they
  used to come back as hexadecimal text. skiLift now asks Snowflake for hex
  explicitly on every statement, so an account whose default is `BASE64` is
  read correctly too. **This changes what existing queries return.**

* **`dbWriteTable()` and `dbAppendTable()` corrupted text containing a
  backslash, lost digits from decimals, and wrote timestamps in your local
  time; all are fixed. If you have written data with an earlier version
  (including `RSnowflake`), check it.** Snowflake treats a backslash inside a
  quoted string as an escape character, which skiLift did not allow for, so
  `C:\temp\new` was stored as `C:`, a tab, `emp`, a newline, and so on, and a
  literal `\n` in text became a real newline. `dbQuoteString()` had the same
  fault, which also affected dbplyr filters on such strings. Separately:
  * decimals were written with 15 significant digits, so `0.1 + 0.2` and
    numbers such as `123456789.123456789` did not round-trip exactly (now 17
    where 15 is not enough);
  * `TIMESTAMP` values were written as your R session's local clock time, so
    they shifted by your offset from UTC and lost fractional seconds. They are
    now written as UTC with microseconds;
  * `NaN`, `Inf` and `-Inf` produced invalid SQL, and `difftime` / `hms` times
    and `blob` binary values were written incorrectly. All are now written
    as proper literals.

* **`TIME` and `TIMESTAMP_TZ` columns returned wrong results; both are fixed.
  If you have read these column types with this package (or with `RSnowflake`,
  which has the same fault in every earlier release), check any results you
  kept.**
  * `TIME` values came back about a billion times too small -- 12:34:56 was
    returned as 00:00:00.000045. The code treated the number Snowflake sends
    (seconds since midnight) as nanoseconds.
  * `TIMESTAMP_TZ` values always came back as `NA`. Snowflake sends the epoch
    seconds followed by a UTC offset, and the code could not read that pair.
    The value is now returned as the correct moment in time, shown in UTC; the
    offset it was stored with is not kept.

  `TIMESTAMP_NTZ`, `TIMESTAMP_LTZ`, `DATE`, text, numeric and boolean columns
  were not affected. Both fixes are checked against what the Snowflake SQL API
  actually returns and against the `adbi` driver.

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
