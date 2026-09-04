# SnowflakeResult S4 Class & DBI Methods
# =============================================================================
# Uses an environment-based .state slot for mutable state (fetch tracking),
# since S4 objects have copy-on-modify semantics.

#' SnowflakeResult
#'
#' An S4 class representing the result of a query against Snowflake.
#' Created by [dbSendQuery()] or [dbSendStatement()].
#'
#' @param object A [SnowflakeResult-class] object (for `show`).
#' @param ... Additional arguments (currently ignored).
#' @slot connection The parent SnowflakeConnection.
#' @slot statement The SQL statement string.
#' @slot .resp_body The raw JSON response from the first partition.
#' @slot .meta Parsed metadata (columns, partitions, handle).
#' @slot .state Environment holding mutable state (fetched, partition, valid, etc.).
#' @export
setClass("SnowflakeResult",
  contains = "DBIResult",
  slots = list(
    connection = "SnowflakeConnection",
    statement  = "character",
    .resp_body = "list",
    .meta      = "list",
    .state     = "environment"
  )
)

.new_result_state <- function(rows_affected = -1L) {
  env <- new.env(parent = emptyenv())
  env$valid             <- TRUE
  env$fetched           <- FALSE
  env$current_partition <- 0L
  env$rows_fetched      <- 0L
  env$rows_affected     <- as.integer(rows_affected)
  env
}

#' @rdname SnowflakeResult-class
#' @export
setMethod("show", "SnowflakeResult", function(object) {
  if (object@.state$valid) {
    cat(sprintf(
      "<SnowflakeResult>\n  SQL: %s\n  Rows: %s, Columns: %d\n",
      truncate_sql(object@statement),
      object@.meta$num_rows,
      nrow(object@.meta$columns)
    ))
  } else {
    cat("<SnowflakeResult> CLEARED\n")
  }
})

#' @rdname SnowflakeResult-class
#' @param dbObj A [SnowflakeResult-class] object.
#' @export
setMethod("dbIsValid", "SnowflakeResult", function(dbObj, ...) {
  dbObj@.state$valid
})

#' @rdname SnowflakeResult-class
#' @param res A [SnowflakeResult-class] object.
#' @export
setMethod("dbClearResult", "SnowflakeResult", function(res, ...) {
  res@.state$valid   <- FALSE
  res@.state$fetched <- TRUE
  invisible(TRUE)
})

#' @rdname SnowflakeResult-class
#' @param n Number of rows to fetch. -1 (default) fetches all.
#' @export
setMethod("dbFetch", signature("SnowflakeResult"),
  function(res, n = -1, ...) {
    st <- res@.state
    if (!st$valid) {
      cli_abort("Result has been cleared.")
    }
    if (isTRUE(st$pending)) {
      cli_abort("Query has bind placeholders. Call {.fn dbBind} first.")
    }

    # ADBC-produced results carry a pre-fetched data.frame
    if (!is.null(st$adbc_data) && !st$fetched) {
      df <- st$adbc_data
      if (n > 0 && nrow(df) > n) {
        df <- df[seq_len(n), , drop = FALSE]
      }
      st$fetched <- TRUE
      st$current_partition <- 1L
      st$rows_fetched <- st$rows_fetched + nrow(df)
      return(df)
    }

    meta <- .get_meta(res)
    resp_body <- .get_resp_body(res)
    total_partitions <- max(meta$num_partitions, 1L)

    if (st$fetched && st$current_partition >= total_partitions) {
      return(.empty_df_from_meta(meta))
    }

    all_parts <- list()
    start_part <- if (st$fetched) st$current_partition else 0L

    if (n < 0) {
      if (start_part == 0L) {
        parsed <- sf_parse_response(resp_body)
        all_parts[[1L]] <- parsed$data
        start_part <- 1L
      }

      if (total_partitions > start_part) {
        remaining <- seq.int(start_part, total_partitions - 1L)
        use_parallel <- isTRUE(getOption("skiLift.parallel_fetch", TRUE)) &&
                        length(remaining) > 1L

        if (use_parallel) {
          rest <- sf_fetch_partitions_parallel(
            res@connection, meta$statement_handle, remaining, meta
          )
        } else {
          rest <- .fetch_partitions_sequential(
            res@connection, meta$statement_handle, remaining, meta
          )
        }
        all_parts <- c(all_parts, rest)
      }

      st$fetched <- TRUE
      st$current_partition <- total_partitions

      if (length(all_parts) == 0L) return(.empty_df_from_meta(meta))
      df <- if (length(all_parts) == 1L) all_parts[[1L]] else do.call(rbind, all_parts)
      st$rows_fetched <- st$rows_fetched + nrow(df)
      return(df)
    }

    if (!st$fetched) {
      parsed <- sf_parse_response(resp_body)
      st$fetched <- TRUE
      st$current_partition <- 1L

      df <- parsed$data
      if (n > 0 && nrow(df) > n) {
        df <- df[seq_len(n), , drop = FALSE]
      }
      st$rows_fetched <- st$rows_fetched + nrow(df)
      return(df)
    }

    .empty_df_from_meta(meta)
  }
)

#' @rdname SnowflakeResult-class
#' @export
setMethod("dbHasCompleted", "SnowflakeResult", function(res, ...) {
  st <- res@.state
  if (!st$valid) return(TRUE)
  if (isTRUE(st$pending)) return(FALSE)
  meta <- .get_meta(res)
  total_partitions <- max(meta$num_partitions, 1L)
  st$fetched && st$current_partition >= total_partitions
})

#' @rdname SnowflakeResult-class
#' @export
setMethod("dbFetchArrow", signature("SnowflakeResult"),
  function(res, ...) {
    rlang::check_installed("nanoarrow", reason = "for Arrow result format")
    st <- res@.state
    if (!st$valid) cli_abort("Result has been cleared.")

    meta <- .get_meta(res)
    stream <- sf_fetch_all_as_arrow_stream(res@connection, res@.resp_body, meta)

    st$fetched <- TRUE
    st$current_partition <- max(meta$num_partitions, 1L)
    st$rows_fetched <- st$rows_fetched + meta$num_rows
    stream
  }
)

#' @rdname SnowflakeResult-class
#' @export
setMethod("dbFetchArrowChunk", signature("SnowflakeResult"),
  function(res, ...) {
    rlang::check_installed("nanoarrow", reason = "for Arrow result format")
    st <- res@.state
    if (!st$valid) cli_abort("Result has been cleared.")

    meta <- .get_meta(res)
    total_partitions <- max(meta$num_partitions, 1L)

    if (st$fetched && st$current_partition >= total_partitions) {
      return(nanoarrow::basic_array_stream(list()))
    }

    part_idx <- if (st$fetched) st$current_partition else 0L
    stream <- sf_fetch_partition_as_arrow_stream(
      res@connection, res@.resp_body, meta, part_idx
    )

    st$fetched <- TRUE
    st$current_partition <- part_idx + 1L
    stream
  }
)

#' @rdname SnowflakeResult-class
#' @export
setMethod("dbGetRowCount", "SnowflakeResult", function(res, ...) {
  res@.state$rows_fetched
})

#' @rdname SnowflakeResult-class
#' @export
setMethod("dbGetRowsAffected", "SnowflakeResult", function(res, ...) {
  res@.state$rows_affected
})

#' @rdname SnowflakeResult-class
#' @export
setMethod("dbGetStatement", "SnowflakeResult", function(res, ...) {
  res@statement
})

#' @rdname SnowflakeResult-class
#' @export
setMethod("dbColumnInfo", "SnowflakeResult", function(res, ...) {
  meta <- res@.meta
  data.frame(
    name = meta$columns$name,
    type = meta$columns$type,
    stringsAsFactors = FALSE
  )
})

#' @rdname SnowflakeResult-class
#' @export
setMethod("dbGetInfo", "SnowflakeResult", function(dbObj, ...) {
  list(
    statement = dbObj@statement,
    row.count = dbObj@.state$rows_fetched,
    rows.affected = dbObj@.state$rows_affected,
    has.completed = dbHasCompleted(dbObj),
    is.select = dbObj@.state$rows_affected == -1L
  )
})


# ---------------------------------------------------------------------------
# dbBind
# ---------------------------------------------------------------------------

#' @rdname SnowflakeResult-class
#' @param params A list of parameter values for parameterized queries.
#' @export
setMethod("dbBind", "SnowflakeResult",
  function(res, params, ...) {
    st <- res@.state
    if (!st$valid) cli_abort("Result has been cleared.")

    bindings <- .params_to_bindings(params)
    resp <- sf_api_submit(res@connection, res@statement, bindings = bindings)
    meta <- sf_parse_metadata(resp)

    st$pending <- FALSE
    st$fetched <- FALSE
    st$current_partition <- 0L
    st$rows_fetched <- 0L
    st$rows_affected <- .extract_rows_affected(resp)

    st$resp_body <- resp
    st$meta <- meta
    invisible(res)
  }
)

# Accessors that check for bound data in state env
.get_resp_body <- function(res) {
  st <- res@.state
  if (!is.null(st$resp_body)) st$resp_body else res@.resp_body
}

.get_meta <- function(res) {
  st <- res@.state
  if (!is.null(st$meta)) st$meta else res@.meta
}


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

truncate_sql <- function(sql, max_len = 60L) {
  if (nchar(sql) > max_len) {
    paste0(substr(sql, 1, max_len - 3), "...")
  } else {
    sql
  }
}
