# Bulk Upload Optimisations
# =============================================================================
# The SQL API v2 REST endpoint does NOT support array bind parameters like
# JDBC/ODBC do (confirmed by Snowflake engineering).  The v2 `bindings` field
# is a simple map of scalar binds -- there is no multi-row array-binding
# contract, no stage-based CSV/Arrow upload endpoint, and no support for
# PUT/GET/USE/ALTER SESSION.  Individual bind parameters add significant
# per-parameter overhead on the server side, making the literal SQL VALUES
# path faster for bulk inserts via REST.
#
# Upload method routing (see .insert_data in connection.R):
#   "auto"    -- ADBC bulk ingest when available AND cell count >= threshold,
#                otherwise literal SQL INSERT.  This is the default.
#   "adbc"    -- Force ADBC; falls back to literal if ADBC unavailable.
#   "literal" -- SQL string INSERT (fastest REST-only path).
#   "bind"    -- DEPRECATED: bind-parameter INSERT.  Slower than literal on
#                SQL API v2 (no array binding).  Kept only for backwards
#                compatibility; will emit a deprecation warning.

#' Insert data via bind-parameter INSERT
#'
#' Generates `INSERT INTO t (cols) VALUES (?, ?), (?, ?), ...` and passes
#' actual values through the SQL API v2 `bindings` field, numbered
#' sequentially in row-major order.
#'
#' Column types and string values are computed vectorised per column,
#' then interleaved into the flat row-major binding list.
#'
#' @param conn SnowflakeConnection.
#' @param table_id Quoted table identifier string.
#' @param df data.frame to insert.
#' @param batch_size Integer rows per batch.
#' @noRd
.insert_data_bind <- function(conn, table_id, df, batch_size = NULL) {
  if (is.null(batch_size)) {
    batch_size <- as.integer(getOption("skiLift.insert_batch_size", 16384L))
  }
  n <- nrow(df)
  ncols <- ncol(df)

  col_clause <- paste0(
    " (",
    paste(vapply(names(df), function(nm) {
      dbQuoteIdentifier(conn, nm)
    }, character(1)), collapse = ", "),
    ")"
  )

  placeholders <- paste(rep("?", ncols), collapse = ", ")
  values_clause <- paste0("(", placeholders, ")")

  use_progress <- n > batch_size && requireNamespace("cli", quietly = TRUE)
  if (use_progress) {
    pb <- cli::cli_progress_bar(
      total   = n,
      format  = "Inserting rows {cli::pb_current}/{cli::pb_total} {cli::pb_bar} {cli::pb_percent}"
    )
  }

  for (start in seq(1L, n, by = batch_size)) {
    end_row <- min(start + batch_size - 1L, n)
    batch_n <- end_row - start + 1L

    all_values <- paste(rep(values_clause, batch_n), collapse = ",\n")
    sql <- paste0("INSERT INTO ", table_id, col_clause, " VALUES\n", all_values)

    bindings <- .df_slice_to_bindings(df, start, end_row)
    sf_api_submit(conn, sql, bindings = bindings)

    if (use_progress) cli::cli_progress_update(set = end_row, id = pb)
  }

  if (use_progress) cli::cli_progress_done(id = pb)
}


#' Vectorised conversion of a df slice to flat row-major bindings
#'
#' Computes column types and string values once per column (vectorised),
#' then interleaves into the flat sequential numbering that the API expects:
#' parameter "1" = row1/col1, "2" = row1/col2, ..., "C+1" = row2/col1, etc.
#'
#' @param df data.frame.
#' @param start First row index (1-based).
#' @param end_row Last row index (1-based).
#' @returns Named list of bindings.
#' @noRd
.df_slice_to_bindings <- function(df, start, end_row) {
  batch <- df[start:end_row, , drop = FALSE]
  nrows <- nrow(batch)
  ncols <- ncol(batch)
  total <- nrows * ncols

  col_types <- character(ncols)
  col_vals  <- vector("list", ncols)

  for (j in seq_len(ncols)) {
    col <- batch[[j]]
    col_types[j] <- .r_col_to_sf_bind_type(col)
    col_vals[[j]] <- .col_to_string_values(col)
  }

  # Build flat row-major list
  bindings <- vector("list", total)
  idx <- 0L
  for (i in seq_len(nrows)) {
    for (j in seq_len(ncols)) {
      idx <- idx + 1L
      bindings[[idx]] <- list(type = col_types[j], value = col_vals[[j]][[i]])
    }
  }
  names(bindings) <- as.character(seq_len(total))
  bindings
}


#' Determine the Snowflake binding type for an R column
#' @noRd
.r_col_to_sf_bind_type <- function(col) {
  if (is.logical(col))          return("BOOLEAN")
  if (is.integer(col))          return("FIXED")
  if (inherits(col, "Date"))    return("TEXT")
  if (inherits(col, "POSIXct")) return("TEXT")
  if (is.numeric(col))          return("REAL")
  "TEXT"
}


#' Vectorised conversion of an R column to string values for binding
#'
#' Returns a list where non-NA values are character strings and NA values
#' are NULL (which serialises to JSON null).
#' @noRd
.col_to_string_values <- function(col) {
  if (is.logical(col)) {
    strs <- ifelse(col, "TRUE", "FALSE")
  } else if (inherits(col, "Date")) {
    strs <- format(col, "%Y-%m-%d")
  } else if (inherits(col, "POSIXct")) {
    strs <- format(col, "%Y-%m-%d %H:%M:%OS3", tz = "UTC")
  } else {
    strs <- as.character(col)
  }

  result <- as.list(strs)
  na_mask <- is.na(col)
  if (any(na_mask)) {
    result[na_mask] <- list(NULL)
  }
  result
}
