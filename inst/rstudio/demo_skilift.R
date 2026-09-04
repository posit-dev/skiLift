# =============================================================================
# skiLift -- Feature Demo (RStudio / Positron)
# =============================================================================
#
# First-time setup (from the project root in RStudio):
#   renv::restore()                # install all pinned dependencies
#   renv::install("./skiLift")  # install package from local source
#
# An interactive walkthrough of skiLift's key features:
#   1. DBI connection via connections.toml
#   2. Querying (dbGetQuery, dbSendQuery/dbFetch)
#   3. Table operations (write, read, append, remove)
#   4. Identifier case handling
#   5. Parameterized queries
#   6. Transactions
#   7. dbplyr / dplyr lazy evaluation
#   8. Arrow fast path
#   9. Connections Pane browsing (dbListObjects)
#
# Intended to be run section-by-section in RStudio (Cmd+Enter per block).
# =============================================================================

library(DBI)
library(skiLift)

# ── 1. Connect ──────────────────────────────────────────────────────────────

# Uses a connections.toml profile (JWT key-pair auth -- no password or MFA required)
# Override: Sys.setenv(SKILIFT_PROFILE = "my_profile")
profile <- Sys.getenv("SKILIFT_PROFILE", "default")
con <- dbConnect(Snowflake(), name = profile)
con
dbGetInfo(con)


# ── 2. Simple Queries ───────────────────────────────────────────────────────

dbGetQuery(con, "SELECT CURRENT_VERSION() AS version")

dbGetQuery(con, "
  SELECT
    42            AS int_val,
    3.14::DOUBLE  AS dbl_val,
    'hello'       AS str_val,
    TRUE          AS bool_val,
    CURRENT_DATE()          AS date_val,
    CURRENT_TIMESTAMP()     AS ts_val
")


# ── 3. Table Operations ────────────────────────────────────────────────────

# Create a demo data.frame with lowercase column names.
# By default, skiLift uppercases identifiers (ODBC-compatible behavior).
# Set options(skiLift.identifier_case = "preserve") to keep original case.
demo <- data.frame(
  id     = 1:10,
  city   = c("London", "Paris", "Tokyo", "Sydney", "NYC",
             "Berlin", "Toronto", "Mumbai", "Seoul", "Dubai"),
  temp_c = c(12.5, 15.2, 22.3, 25.1, 18.7,
             10.3, 8.9, 33.2, 19.8, 38.5),
  rainy  = c(TRUE, TRUE, FALSE, FALSE, TRUE,
             TRUE, TRUE, FALSE, FALSE, FALSE),
  stringsAsFactors = FALSE
)

dbWriteTable(con, "DEMO_CITIES", demo, overwrite = TRUE)

# Column names are uppercased by default (ODBC-compatible)
dbListFields(con, "DEMO_CITIES")  # -> c("ID", "CITY", "TEMP_C", "RAINY")

# Read it back
dbReadTable(con, "DEMO_CITIES")

# Append more rows
extra <- data.frame(
  id = 11:12,
  city = c("Rome", "Cairo"),
  temp_c = c(20.1, 35.0),
  rainy = c(FALSE, FALSE)
)
dbAppendTable(con, "DEMO_CITIES", extra)

dbGetQuery(con, "SELECT COUNT(*) AS n FROM DEMO_CITIES")


# ── 4. Identifier Case Handling ─────────────────────────────────────────────

# By default, skiLift uppercases identifiers to match ODBC behavior.
# In raw SQL, reference columns in uppercase (quoted or unquoted):
dbGetQuery(con, 'SELECT "CITY", "TEMP_C" FROM DEMO_CITIES WHERE "TEMP_C" > 25')

# Unquoted also works since Snowflake uppercases unquoted identifiers:
dbGetQuery(con, "SELECT CITY, TEMP_C FROM DEMO_CITIES WHERE TEMP_C > 25")

# dbQuoteIdentifier wraps names in double-quotes (does NOT change case):
dbQuoteIdentifier(con, "myColumn")  # -> "myColumn"

# dbUnquoteIdentifier parses back:
dbUnquoteIdentifier(con, SQL('"mydb"."myschema"."mytable"'))


# ── 5. Parameterized Queries ───────────────────────────────────────────────

# Use ? placeholders with the params argument:
dbGetQuery(
  con,
  'SELECT * FROM DEMO_CITIES WHERE "TEMP_C" > ?',
  params = list(30)
)

# Or via dbBind:
res <- dbSendQuery(con, 'SELECT * FROM DEMO_CITIES WHERE "CITY" = ?')
dbBind(res, list("Tokyo"))
dbFetch(res)
dbClearResult(res)


# ── 6. Transactions ────────────────────────────────────────────────────────

# Transaction support requires session-based state (not yet available via
# SQL API v2). This section will work once transactions are implemented.
tryCatch({
  dbBegin(con)
  dbExecute(con, "INSERT INTO \"DEMO_CITIES\" (\"ID\",\"CITY\",\"TEMP_C\",\"RAINY\") VALUES (13, 'Lima', 22.0, FALSE)")
  dbRollback(con)

  dbGetQuery(con, "SELECT COUNT(*) AS n FROM DEMO_CITIES")

  dbWithTransaction(con, {
    dbExecute(con, "INSERT INTO \"DEMO_CITIES\" (\"ID\",\"CITY\",\"TEMP_C\",\"RAINY\") VALUES (14, 'Oslo', 5.0, TRUE)")
    stop("Simulated error -- transaction will roll back")
  })
}, error = function(e) {
  message("Transactions not yet supported: ", conditionMessage(e))
})

dbGetQuery(con, "SELECT COUNT(*) AS n FROM DEMO_CITIES")


# ── 7. dbplyr / dplyr Integration ──────────────────────────────────────────

if (requireNamespace("dbplyr", quietly = TRUE) &&
    requireNamespace("dplyr", quietly = TRUE)) {

  library(dplyr)

  cities_tbl <- tbl(con, "DEMO_CITIES")
  cities_tbl

  # Lazy query -- translated to Snowflake SQL, not executed yet
  # Column names are uppercase (ODBC-compat default)
  hot_cities <- cities_tbl |>
    filter(TEMP_C > 20) |>
    select(CITY, TEMP_C) |>
    arrange(desc(TEMP_C))

  # See the generated SQL
  show_query(hot_cities)

  # Execute and pull into R
  hot_cities |> collect()

  # Aggregation
  cities_tbl |>
    summarise(
      avg_temp = mean(TEMP_C, na.rm = TRUE),
      n_rainy  = sum(as.integer(RAINY), na.rm = TRUE),
      n_cities = n()
    ) |>
    collect()

} else {
  message("Install dbplyr and dplyr for this section: install.packages(c('dbplyr', 'dplyr'))")
}


# ── 8. Arrow Fast Path (optional) ──────────────────────────────────────────

if (requireNamespace("nanoarrow", quietly = TRUE)) {
  stream <- dbGetQueryArrow(con, "SELECT * FROM DEMO_CITIES")
  arrow_df <- as.data.frame(stream)
  str(arrow_df)
  cat("Arrow result:", nrow(arrow_df), "rows,", ncol(arrow_df), "columns\n")
} else {
  message("Install nanoarrow for Arrow fast path: install.packages('nanoarrow')")
}


# ── 9. Connections Pane Browsing ────────────────────────────────────────────

# dbListObjects powers the RStudio Connections Pane hierarchy.
# NULL prefix -> databases:
head(dbListObjects(con))

# 1-component prefix -> schemas in a database:
dbListObjects(con, prefix = Id(catalog = "SKILIFT"))

# 2-component prefix -> tables in a schema:
dbListObjects(con, prefix = Id(catalog = "SKILIFT", schema = "PUBLIC"))


# ── 10. Cleanup ─────────────────────────────────────────────────────────────

dbRemoveTable(con, "DEMO_CITIES")
dbDisconnect(con)
cat("Done! Connection closed.\n")
