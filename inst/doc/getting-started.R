## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)


## ----install------------------------------------------------------------------
# From GitHub (development version):
# install.packages("pak")
pak::pak("posit-dev/skiLift")


## ----connect-toml-------------------------------------------------------------
library(DBI)
library(skiLift)

con <- dbConnect(Snowflake())


## ----connect-named------------------------------------------------------------
con <- dbConnect(Snowflake(), name = "my_profile")


## ----connect-pat--------------------------------------------------------------
con <- dbConnect(
  Snowflake(),
  account = "myaccount",
  token   = Sys.getenv("SNOWFLAKE_PAT")
)


## ----connect-workspace--------------------------------------------------------
con <- dbConnect(Snowflake())


## ----query--------------------------------------------------------------------
# Simple query
df <- dbGetQuery(con, "SELECT * FROM my_table LIMIT 10")

# Parameterized query
df <- dbGetQuery(
  con,
  "SELECT * FROM users WHERE age > ?",
  params = list(21L)
)

# Execute DDL / DML (returns rows affected)
dbExecute(con, "CREATE TABLE test_tbl (id INT, name VARCHAR)")
dbExecute(con, "INSERT INTO test_tbl VALUES (1, 'Alice')")


## ----tables-------------------------------------------------------------------
# Write a data.frame to Snowflake
dbWriteTable(con, "iris_copy", iris)

# Read it back
iris_back <- dbReadTable(con, "iris_copy")

# Append more rows
dbAppendTable(con, "iris_copy", iris[1:10, ])

# Clean up
dbRemoveTable(con, "iris_copy")


## ----upload-method------------------------------------------------------------
# Force a specific method
options(skiLift.upload_method = "snowpark")
dbWriteTable(con, "big_table", large_df)

# Adjust the threshold (cells = rows * cols)
options(skiLift.bulk_write_threshold = 100000L)


## ----case-upper---------------------------------------------------------------
dbWriteTable(con, "my_table", data.frame(id = 1, name = "Alice"))
dbListFields(con, "my_table")
#> [1] "ID" "NAME"


## ----case-preserve------------------------------------------------------------
options(skiLift.identifier_case = "preserve")

dbWriteTable(con, "my_table", data.frame(id = 1, name = "Alice"), overwrite = TRUE)
dbListFields(con, "my_table")
#> [1] "id" "name"


## ----adbc---------------------------------------------------------------------
# Check if ADBC is available
has_adbc <- requireNamespace("adbcsnowflake", quietly = TRUE)
cat("ADBC available:", has_adbc, "\n")

# ADBC is used automatically in 'auto' mode for large writes
# Force it for reads:
options(skiLift.backend = "adbc")
df <- dbGetQuery(con, "SELECT * FROM big_table")
options(skiLift.backend = "auto")


## ----arrow--------------------------------------------------------------------
stream <- dbGetQueryArrow(con, "SELECT * FROM my_table")
df <- as.data.frame(stream)


## ----dbplyr-------------------------------------------------------------------
library(dplyr)

tbl(con, "my_table") |>
  filter(score > 90) |>
  select(name, score) |>
  arrange(desc(score)) |>
  collect()


## ----dbplyr-snowflake---------------------------------------------------------
# Example: parse JSON and aggregate into arrays
tbl(con, "events") |>
  mutate(payload = parse_json(raw_json)) |>
  group_by(event_type) |>
  summarise(
    user_ids = array_unique_agg(user_id),
    n_approx = approx_count_distinct(user_id)
  ) |>
  collect()


## ----transactions, eval = FALSE-----------------------------------------------
## # Not yet supported via SQL API v2 -- included for reference:
## dbBegin(con)
## dbExecute(con, "INSERT INTO accounts VALUES (1, 100.00)")
## dbCommit(con)


## ----disconnect---------------------------------------------------------------
dbDisconnect(con)

