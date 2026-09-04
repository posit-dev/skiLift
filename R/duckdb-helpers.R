# DuckDB Caching Helpers (Companion, Not Embedded)
# =============================================================================
# DuckDB is NOT part of the DBI driver -- it is a separate workflow layer.
# These convenience functions bridge skiLift and DuckDB for the common
# pattern: "query Snowflake once, analyse locally with dplyr."
#
# All functions require the `duckdb` package (listed in Suggests).

#' Cache a Snowflake query result in a local DuckDB table
#'
#' Executes `sql` against Snowflake (using the best available backend --
#' ADBC if installed, REST API v2 otherwise) and writes the result into a
#' DuckDB table for fast local analysis with \code{dplyr} / \code{dbplyr}.
#'
#' @param sf_con A \code{SnowflakeConnection} object.
#' @param sql Character SQL query to run against Snowflake.
#' @param table_name Character name for the DuckDB table.
#' @param duck_con An existing DuckDB connection, or \code{NULL} to create one.
#' @param overwrite Logical; overwrite if the table already exists? Default \code{TRUE}.
#' @returns The DuckDB connection (invisibly).  If \code{duck_con} was
#'   \code{NULL}, the caller is responsible for disconnecting it.
#' @export
sf_cache_in_duckdb <- function(sf_con, sql, table_name,
                               duck_con = NULL, overwrite = TRUE) {
  rlang::check_installed("duckdb", reason = "for local DuckDB caching")

  if (is.null(duck_con)) {
    duck_con <- DBI::dbConnect(duckdb::duckdb())
  }

  data <- DBI::dbGetQuery(sf_con, sql)
  DBI::dbWriteTable(duck_con, table_name, data, overwrite = overwrite)

  if (isTRUE(getOption("skiLift.verbose", FALSE))) {
    cli_inform(c(
      "i" = "Cached {nrow(data)} rows into DuckDB table {.val {table_name}}."
    ))
  }

  invisible(duck_con)
}


#' Materialise a \code{dplyr} lazy table from Snowflake into DuckDB
#'
#' Takes a \code{tbl} backed by a Snowflake connection, collects the
#' underlying SQL, executes it against Snowflake, and writes the result
#' into DuckDB for continued \code{dplyr} analysis.
#'
#' @param .tbl A \code{tbl_lazy} backed by a \code{SnowflakeConnection}.
#' @param table_name Character name for the DuckDB table.
#' @param duck_con An existing DuckDB connection, or \code{NULL} to create one.
#' @param overwrite Logical; overwrite if the table already exists? Default \code{TRUE}.
#' @returns A \code{tbl} backed by DuckDB, ready for further \code{dplyr} ops.
#' @export
sf_collect_to_duckdb <- function(.tbl, table_name,
                                 duck_con = NULL, overwrite = TRUE) {
  rlang::check_installed("duckdb", reason = "for local DuckDB caching")
  rlang::check_installed("dbplyr", reason = "for lazy table SQL extraction")

  sql <- dbplyr::sql_render(.tbl)
  sf_con <- dbplyr::remote_con(.tbl)

  if (is.null(duck_con)) {
    duck_con <- DBI::dbConnect(duckdb::duckdb())
  }

  data <- DBI::dbGetQuery(sf_con, as.character(sql))
  DBI::dbWriteTable(duck_con, table_name, data, overwrite = overwrite)

  dplyr::tbl(duck_con, table_name)
}


#' List tables cached in a DuckDB connection
#'
#' @param duck_con A DuckDB connection.
#' @returns Character vector of table names.
#' @export
sf_duckdb_tables <- function(duck_con) {
  DBI::dbListTables(duck_con)
}
