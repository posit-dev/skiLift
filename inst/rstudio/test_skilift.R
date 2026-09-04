# =============================================================================
# skiLift -- Local Test Script (RStudio / Positron / Cursor)
# =============================================================================
#
# This script mirrors the Workspace Notebook test suite but runs locally.
# It validates DBI operations, type roundtrips, quoting, Connections Pane
# readiness, and the Arrow fast path.
#
# First-time setup (from the project root in RStudio):
#   renv::restore()                # install all pinned dependencies
#   renv::install("./skiLift")  # install package from local source
#
# Prerequisites:
#   1. Open the top-level project folder in RStudio (renv activates automatically)
#   2. Ensure a connections.toml profile is configured
#      (default: ~/.snowflake/connections.toml  [default])
#   3. Key-pair JWT auth configured (no MFA needed)
#   4. Override profile: Sys.setenv(SKILIFT_PROFILE = "my_profile")
#
# Usage:
#   source("skiLift/inst/rstudio/test_skilift.R")
#   -- OR open in RStudio and Run All
# =============================================================================

library(DBI)
library(skiLift)

# ---------------------------------------------------------------------------
# Test harness
# ---------------------------------------------------------------------------
tests_passed <- 0L
tests_failed <- 0L

check <- function(name, condition) {
  if (isTRUE(condition)) {
    cat("  PASS:", name, "\n")
    tests_passed <<- tests_passed + 1L
  } else {
    cat("  FAIL:", name, "\n")
    tests_failed <<- tests_failed + 1L
  }
}

# ---------------------------------------------------------------------------
# 1. Connect
# ---------------------------------------------------------------------------
cat("== 1. Connect ==\n")

profile <- Sys.getenv("SKILIFT_PROFILE", "default")
con <- dbConnect(Snowflake(), name = profile)
check("dbConnect succeeds", dbIsValid(con))
print(con)

info <- dbGetInfo(con)
cat("  Account:  ", info$host, "\n")
cat("  Database: ", info$dbname, "\n")
cat("  Schema:   ", info$schema, "\n")
cat("  Warehouse:", info$warehouse, "\n")

# ---------------------------------------------------------------------------
# 2. Scalar queries & type mapping
# ---------------------------------------------------------------------------
cat("\n== 2. Scalar Queries & Type Mapping ==\n")

df <- dbGetQuery(con, "SELECT 42 AS int_col")
check("integer column", is.integer(df$INT_COL) && df$INT_COL == 42L)

df <- dbGetQuery(con, "SELECT 3.14::DOUBLE AS dbl_col")
check("double column", is.double(df$DBL_COL) && abs(df$DBL_COL - 3.14) < 0.01)

df <- dbGetQuery(con, "SELECT 'hello' AS str_col")
check("string column", is.character(df$STR_COL) && df$STR_COL == "hello")

df <- dbGetQuery(con, "SELECT TRUE AS bool_col")
check("boolean column", is.logical(df$BOOL_COL) && df$BOOL_COL == TRUE)

df <- dbGetQuery(con, "SELECT '2024-06-15'::DATE AS date_col")
check("date column", inherits(df$DATE_COL, "Date"))

df <- dbGetQuery(con, "SELECT '2024-06-15 10:30:00'::TIMESTAMP_NTZ AS ts_col")
check("timestamp column", inherits(df$TS_COL, "POSIXct"))

df <- dbGetQuery(con, "SELECT NULL::INTEGER AS null_col")
check("NULL returns NA", is.na(df$NULL_COL))

# ---------------------------------------------------------------------------
# 3. Table operations
# ---------------------------------------------------------------------------
cat("\n== 3. Table Operations ==\n")

test_df <- data.frame(
  id     = 1:5,
  name   = c("Alice", "Bob", "Carol", "Dave", "Eve"),
  score  = c(95.5, 87.3, 92.1, 78.9, 88.4),
  active = c(TRUE, FALSE, TRUE, TRUE, FALSE),
  stringsAsFactors = FALSE
)

dbWriteTable(con, "SKILIFT_LOCAL_TEST", test_df, overwrite = TRUE)
check("dbWriteTable succeeds", TRUE)

check("dbExistsTable finds it", dbExistsTable(con, "SKILIFT_LOCAL_TEST"))

tables <- dbListTables(con)
check("dbListTables includes table",
      "SKILIFT_LOCAL_TEST" %in% toupper(tables))

fields <- dbListFields(con, "SKILIFT_LOCAL_TEST")
check("dbListFields returns 4 columns", length(fields) == 4)
check("dbListFields returns uppercase (ODBC-compat default)",
      identical(fields, c("ID", "NAME", "SCORE", "ACTIVE")))
cat("  Fields:", paste(fields, collapse = ", "), "\n")

df_read <- dbReadTable(con, "SKILIFT_LOCAL_TEST")
check("dbReadTable returns 5 rows", nrow(df_read) == 5)
check("integer roundtrip", is.integer(df_read$ID))
check("string roundtrip", df_read$NAME[1] == "Alice")

extra <- data.frame(id = 6L, name = "Frank", score = 91.0, active = TRUE)
dbWriteTable(con, "SKILIFT_LOCAL_TEST", extra, append = TRUE)
df_after <- dbReadTable(con, "SKILIFT_LOCAL_TEST")
check("append adds rows", nrow(df_after) == 6)

# ---------------------------------------------------------------------------
# 4. dbSendQuery / dbFetch / dbClearResult
# ---------------------------------------------------------------------------
cat("\n== 4. Streaming Results ==\n")

sql <- 'SELECT * FROM SKILIFT_LOCAL_TEST ORDER BY "ID"'
res <- dbSendQuery(con, sql)
check("dbSendQuery returns SnowflakeResult", is(res, "SnowflakeResult"))
check("result is valid", dbIsValid(res))

df <- dbFetch(res)
check("dbFetch returns 6 rows", nrow(df) == 6)

df2 <- dbFetch(res)
check("second dbFetch returns 0 rows (S4 mutability)", nrow(df2) == 0)
check("dbHasCompleted", dbHasCompleted(res))
check("dbGetStatement", dbGetStatement(res) == sql)
check("dbColumnInfo returns 4 cols", nrow(dbColumnInfo(res)) == 4)

dbClearResult(res)
check("result cleared", !dbIsValid(res))

# ---------------------------------------------------------------------------
# 5. Quoting & identifiers
# ---------------------------------------------------------------------------
cat("\n== 5. Quoting ==\n")

check("dbQuoteIdentifier",
      as.character(dbQuoteIdentifier(con, "my_table")) == '"my_table"')

check("dbQuoteString",
      as.character(dbQuoteString(con, "it's")) == "'it''s'")

check("dbQuoteString NA",
      as.character(dbQuoteString(con, NA_character_)) == "NULL")

check("dbQuoteLiteral int",
      as.character(dbQuoteLiteral(con, 42L)) == "42")

check("dbQuoteLiteral date",
      grepl("DATE", as.character(dbQuoteLiteral(con, as.Date("2024-01-15")))))

# ---------------------------------------------------------------------------
# 6. dbUnquoteIdentifier
# ---------------------------------------------------------------------------
cat("\n== 6. dbUnquoteIdentifier ==\n")

ids <- dbUnquoteIdentifier(con, SQL('"mydb"."myschema"."mytable"'))
check("parses 3-part identifier", length(ids) == 1)
check("catalog = mydb", ids[[1]]@name[["catalog"]] == "mydb")
check("schema = myschema", ids[[1]]@name[["schema"]] == "myschema")
check("table = mytable", ids[[1]]@name[["table"]] == "mytable")

# ---------------------------------------------------------------------------
# 7. DML / dbExecute
# ---------------------------------------------------------------------------
cat("\n== 7. DML ==\n")

affected <- dbExecute(con, 'DELETE FROM SKILIFT_LOCAL_TEST WHERE "ID" = 6')
check("dbExecute DELETE returns affected", affected >= 1)

df <- dbGetQuery(con, "SELECT COUNT(*) AS cnt FROM SKILIFT_LOCAL_TEST")
check("row deleted", df$CNT == 5)

# ---------------------------------------------------------------------------
# 8. Transactions (may not be supported yet via SQL API v2)
# ---------------------------------------------------------------------------
cat("\n== 8. Transactions ==\n")

tryCatch({
  dbBegin(con)
  dbExecute(con, "INSERT INTO \"SKILIFT_LOCAL_TEST\" (\"ID\", \"NAME\", \"SCORE\", \"ACTIVE\") VALUES (7, 'Grace', 99.0, TRUE)")
  dbCommit(con)
  df <- dbGetQuery(con, "SELECT COUNT(*) AS cnt FROM SKILIFT_LOCAL_TEST")
  check("transaction commit", df$CNT == 6)

  dbBegin(con)
  dbExecute(con, "INSERT INTO \"SKILIFT_LOCAL_TEST\" (\"ID\", \"NAME\", \"SCORE\", \"ACTIVE\") VALUES (8, 'Heidi', 77.0, FALSE)")
  dbRollback(con)
  df <- dbGetQuery(con, "SELECT COUNT(*) AS cnt FROM SKILIFT_LOCAL_TEST")
  check("transaction rollback", df$CNT == 6)
}, error = function(e) {
  cat("  SKIP: Transactions not yet supported --", conditionMessage(e), "\n")
})

# ---------------------------------------------------------------------------
# 9. Parameterized queries (dbBind)
# ---------------------------------------------------------------------------
cat("\n== 9. Parameterized Queries ==\n")

res <- dbSendQuery(con, "SELECT * FROM SKILIFT_LOCAL_TEST WHERE \"ID\" = ?")
dbBind(res, list(1L))
df <- dbFetch(res)
dbClearResult(res)
check("parameterized query returns 1 row", nrow(df) == 1)
check("parameterized query correct value", df$NAME == "Alice")

# ---------------------------------------------------------------------------
# 10. dbListObjects (Connections Pane readiness)
# ---------------------------------------------------------------------------
cat("\n== 10. dbListObjects ==\n")

objs <- dbListObjects(con)
check("dbListObjects returns data.frame", is.data.frame(objs))
check("has 'table' and 'is_prefix' columns",
      all(c("table", "is_prefix") %in% names(objs)))
check("top-level are all prefixes (databases)", all(objs$is_prefix))
cat("  Databases found:", nrow(objs), "\n")

# ---------------------------------------------------------------------------
# 11. Arrow fast path (optional)
# ---------------------------------------------------------------------------
cat("\n== 11. Arrow (optional) ==\n")

if (requireNamespace("nanoarrow", quietly = TRUE)) {
  stream <- dbGetQueryArrow(con, "SELECT * FROM SKILIFT_LOCAL_TEST")
  arrow_df <- as.data.frame(stream)
  check("dbGetQueryArrow returns data", nrow(arrow_df) > 0)
  check("Arrow column count matches", ncol(arrow_df) == 4)
} else {
  cat("  SKIP: nanoarrow not installed\n")
}

# ---------------------------------------------------------------------------
# 12. Cleanup
# ---------------------------------------------------------------------------
cat("\n== 12. Cleanup ==\n")

dbRemoveTable(con, "SKILIFT_LOCAL_TEST")
check("table removed", !dbExistsTable(con, "SKILIFT_LOCAL_TEST"))

dbDisconnect(con)
check("disconnected", !dbIsValid(con))

# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------
cat(sprintf("\n============================\n"))
cat(sprintf("  Results: %d passed, %d failed\n", tests_passed, tests_failed))
cat(sprintf("============================\n"))

if (tests_failed > 0) {
  warning(sprintf("%d test(s) failed!", tests_failed))
}

