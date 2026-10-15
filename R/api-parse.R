# SQL API v2 JSON Result Parsing
# =============================================================================

#' Parse a SQL API v2 response into column metadata and R data.frame
#'
#' @param resp_body Parsed JSON body from sf_api_submit / sf_api_fetch_partition.
#' @param bigint The connection's `bigint` setting (NULL for the default).
#' @returns A list with `meta` (column info) and `data` (data.frame).
#' @noRd
sf_parse_response <- function(resp_body, bigint = NULL) {
  meta <- sf_parse_metadata(resp_body, bigint = bigint)

  raw_data <- resp_body$data
  if (is.null(raw_data) || length(raw_data) == 0L) {
    df <- .empty_df_from_meta(meta)
    return(list(meta = meta, data = df))
  }

  df <- .json_data_to_df(raw_data, meta)
  list(meta = meta, data = df)
}


#' Extract column metadata from resultSetMetaData
#'
#' Handles both the nested-list format from httr2::resp_body_json() and the
#' simplified data.frame format from RcppSimdJson::fparse().
#'
#' @param resp_body Parsed JSON body.
#' @returns A list with `num_rows`, `num_partitions`, `columns` (data.frame
#'   of name, type, nullable, scale, precision, length).
#' @noRd
sf_parse_metadata <- function(resp_body, bigint = NULL) {
  rsmd <- resp_body$resultSetMetaData
  if (is.null(rsmd)) {
    return(list(
      num_rows = 0L,
      num_partitions = 0L,
      statement_handle = resp_body$statementHandle %||% "",
      columns = data.frame(
        name = character(0), type = character(0),
        nullable = logical(0), scale = integer(0),
        stringsAsFactors = FALSE
      )
    ))
  }

  row_types <- rsmd$rowType

  if (is.data.frame(row_types)) {
    # fparse simplified the array-of-objects into a data.frame
    cols <- data.frame(
      name      = as.character(row_types$name),
      type      = tolower(as.character(row_types$type)),
      nullable  = as.logical(row_types$nullable),
      scale     = as.integer(row_types$scale),
      precision = as.integer(row_types$precision),
      stringsAsFactors = FALSE
    )
  } else {
    # httr2/jsonlite returns a list of named lists
    if (is.null(row_types)) row_types <- list()
    cols <- data.frame(
      name     = vapply(row_types, function(x) x$name %||% "", character(1)),
      type     = vapply(row_types, function(x) tolower(x$type %||% "text"), character(1)),
      nullable = vapply(row_types, function(x) isTRUE(x$nullable), logical(1)),
      scale    = vapply(row_types, function(x) as.integer(x$scale %||% 0L), integer(1)),
      precision = vapply(row_types, function(x) as.integer(x$precision %||% 0L), integer(1)),
      stringsAsFactors = FALSE
    )
  }

  n_partitions <- 0L
  part_info <- rsmd$partitionInfo
  if (!is.null(part_info)) {
    # fparse simplifies the array-of-objects to a data.frame (use nrow),
    # while httr2/jsonlite returns a list-of-lists (use length).
    n_partitions <- if (is.data.frame(part_info)) nrow(part_info) else length(part_info)
  }

  list(
    bigint           = bigint %||% "numeric",
    num_rows         = as.integer(rsmd$numRows %||% 0L),
    num_partitions   = n_partitions,
    statement_handle = resp_body$statementHandle %||% "",
    statement_status_url = resp_body$statementStatusUrl %||% "",
    columns          = cols
  )
}


#' Convert the data array from JSON response into an R data.frame
#'
#' Handles three formats produced by different JSON parsers:
#'   - character matrix  (fparse with full simplification, no NULLs)
#'   - data.frame        (fparse, when inner arrays look homogeneous)
#'   - list of lists      (httr2/jsonlite, or fparse with NULLs)
#'
#' @param raw_data Parsed data from the JSON response body.
#' @param meta Metadata from sf_parse_metadata().
#' @returns A data.frame.
#' @noRd
.json_data_to_df <- function(raw_data, meta) {
  col_info <- meta$columns
  ncols <- nrow(col_info)

  if (ncols == 0L) return(data.frame())

  if (is.matrix(raw_data)) {
    # fparse simplified to character matrix -- fastest path
    mat <- raw_data
    storage.mode(mat) <- "character"
  } else if (is.data.frame(raw_data)) {
    # fparse simplified to data.frame -- coerce to character matrix
    mat <- as.matrix(raw_data)
    storage.mode(mat) <- "character"
  } else {
    # list-of-lists from httr2/jsonlite (or fparse when NULLs prevent
    # simplification).  Transpose to character matrix in one pass.
    mat <- do.call(rbind, lapply(raw_data, function(row) {
      vapply(row, function(v) {
        if (is.null(v)) NA_character_ else as.character(v)
      }, character(1))
    }))
  }

  # Ensure mat is 2-dimensional (single-row results can lose dim)
  if (is.null(dim(mat))) {
    mat <- matrix(mat, nrow = 1L)
  }

  columns <- vector("list", ncols)
  names(columns) <- col_info$name

  for (j in seq_len(ncols)) {
    columns[[j]] <- sf_convert_column(
      mat[, j], col_info$type[j], col_info$scale[j], col_info$precision[j],
      meta$bigint %||% "numeric"
    )
  }

  as.data.frame(columns, stringsAsFactors = FALSE, check.names = FALSE)
}


#' Create an empty data.frame with the right column names and types
#' @noRd
.empty_df_from_meta <- function(meta) {
  col_info <- meta$columns
  if (nrow(col_info) == 0L) return(data.frame())

  columns <- vector("list", nrow(col_info))
  names(columns) <- col_info$name

  for (j in seq_len(nrow(col_info))) {
    columns[[j]] <- sf_convert_column(
      character(0), col_info$type[j], col_info$scale[j], col_info$precision[j],
      meta$bigint %||% "numeric"
    )
  }

  as.data.frame(columns, stringsAsFactors = FALSE, check.names = FALSE)
}
