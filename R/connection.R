# SnowflakeConnection S4 Class & DBI Methods
# =============================================================================

#' SnowflakeConnection
#'
#' An S4 class representing a connection to Snowflake via the SQL API v2.
#' Created by [dbConnect()] with a [Snowflake()] driver.
#'
#' @param object A [SnowflakeConnection-class] object (for `show`).
#' @param ... Additional arguments (currently ignored).
#' @slot account Account identifier.
#' @slot user Username.
#' @slot database Default database.
#' @slot schema Default schema.
#' @slot warehouse Default warehouse.
#' @slot role Default role.
#' @slot .auth Auth list (type, token, token_type, plus params for refresh).
#' @slot .state Environment holding mutable state (valid, in_transaction,
#'   session_info, and the live auth token cache -- token, token_mtime).
#' @export
setClass("SnowflakeConnection",
  contains = "DBIConnection",
  slots = list(
    account   = "character",
    user      = "character",
    database  = "character",
    schema    = "character",
    warehouse = "character",
    role      = "character",
    .auth     = "list",
    .state    = "environment"
  )
)

.new_conn_state <- function() {
  env <- new.env(parent = emptyenv())
  env$valid          <- TRUE
  env$in_transaction <- FALSE
  env$session_info   <- NULL
  env$adbc           <- NULL
  # Live auth token cache. .auth is an S4 slot (value-copied), so a refresh
  # written there never reaches the caller -- this environment is the
  # reference-semantics home for the token that actually goes on the wire.
  env$token          <- NULL
  env$token_mtime    <- NULL
  # Same rationale for auth types that hand us a finished header set rather
  # than a raw token (external browser SSO): the live copy has to live here,
  # not in the .auth slot, or a refresh never reaches the caller.
  env$headers        <- NULL
  env
}

#' @rdname SnowflakeConnection-class
#' @export
setMethod("show", "SnowflakeConnection", function(object) {
  if (object@.state$valid) {
    cat(sprintf(
      "<SnowflakeConnection> %s@%s [%s.%s]\n",
      object@user, object@account, object@database, object@schema
    ))
  } else {
    cat("<SnowflakeConnection> DISCONNECTED\n")
  }
})

#' @rdname SnowflakeConnection-class
#' @param dbObj A [SnowflakeConnection-class] object.
#' @export
setMethod("dbIsValid", "SnowflakeConnection", function(dbObj, ...) {
  dbObj@.state$valid
})

#' @rdname SnowflakeConnection-class
#' @param conn A [SnowflakeConnection-class] object.
#' @export
setMethod("dbDisconnect", "SnowflakeConnection", function(conn, ...) {
  if (.has_adbc(conn)) {
    .adbc_cleanup(conn@.state$adbc)
    conn@.state$adbc <- NULL
  }
  if (.has_session(conn)) {
    tryCatch(
      sf_session_delete(conn@account, conn@.state$session),
      error = function(e) NULL
    )
    conn@.state$session <- NULL
  }
  .on_connection_closed(conn)
  conn@.state$valid <- FALSE
  invisible(TRUE)
})

#' @rdname SnowflakeConnection-class
#' @export
setMethod("dbGetInfo", "SnowflakeConnection", function(dbObj, ...) {
  list(
    dbname    = dbObj@database,
    username  = dbObj@user,
    host      = paste0(dbObj@account, ".snowflakecomputing.com"),
    port      = 443L,
    schema    = dbObj@schema,
    warehouse = dbObj@warehouse,
    role      = dbObj@role
  )
})

# ---------------------------------------------------------------------------
# Query execution
# ---------------------------------------------------------------------------

#' @rdname SnowflakeConnection-class
#' @param statement SQL statement string.
#' @param params Optional parameter list for parameterized queries.
#' @export
setMethod("dbSendQuery", signature("SnowflakeConnection", "character"),
  function(conn, statement, params = NULL, ...) {
    .check_valid(conn)

    # Defer execution when the SQL has ? placeholders but no params yet
    # (the caller will supply them via dbBind)
    if (is.null(params) && grepl("\\?", statement)) {
      state <- .new_result_state(rows_affected = -1L)
      state$pending <- TRUE
      return(new("SnowflakeResult",
        connection = conn,
        statement  = statement,
        .resp_body = list(),
        .meta      = list(),
        .state     = state
      ))
    }

    # ADBC fast path for parameterless queries -- eagerly fetch the result
    # so that downstream dbFetch() can return the cached data.frame.
    backend <- getOption("skiLift.backend", "auto")
    if (is.null(params) && backend %in% c("adbc", "auto")) {
      adbc <- .ensure_adbc(conn)
      if (!is.null(adbc)) {
        adbc_df <- tryCatch(
          .adbc_get_query(adbc, statement),
          error = function(e) {
            if (!identical(backend, "auto")) {
              cli_warn(c(
                "!" = "ADBC query failed, falling back to REST API.",
                "i" = conditionMessage(e)
              ))
            }
            NULL
          }
        )
        if (!is.null(adbc_df)) {
          col_info <- data.frame(
            name = names(adbc_df),
            type = vapply(adbc_df, function(col) r_to_sf_type(col), character(1)),
            stringsAsFactors = FALSE
          )
          meta <- list(
            columns          = col_info,
            num_rows         = nrow(adbc_df),
            num_partitions   = 1L,
            statement_handle = ""
          )
          state <- .new_result_state(rows_affected = -1L)
          state$adbc_data <- adbc_df
          return(new("SnowflakeResult",
            connection = conn,
            statement  = statement,
            .resp_body = list(),
            .meta      = meta,
            .state     = state
          ))
        }
      }
    }

    bindings <- if (!is.null(params)) .params_to_bindings(params) else NULL
    resp <- sf_api_submit(conn, statement, bindings = bindings)
    meta <- sf_parse_metadata(resp)

    new("SnowflakeResult",
      connection = conn,
      statement  = statement,
      .resp_body = resp,
      .meta      = meta,
      .state     = .new_result_state(rows_affected = -1L)
    )
  }
)

#' @rdname SnowflakeConnection-class
#' @export
setMethod("dbSendStatement", signature("SnowflakeConnection", "character"),
  function(conn, statement, params = NULL, ...) {
    .check_valid(conn)

    # ADBC fast path for parameterless DDL/DML
    backend <- getOption("skiLift.backend", "auto")
    if (is.null(params) && backend %in% c("adbc", "auto")) {
      adbc <- .ensure_adbc(conn)
      if (!is.null(adbc)) {
        adbc_ok <- tryCatch({
          .adbc_execute_sql(adbc, statement)
          TRUE
        }, error = function(e) {
          if (!identical(backend, "auto")) {
            cli_warn(c(
              "!" = "ADBC statement failed, falling back to REST API.",
              "i" = conditionMessage(e)
            ))
          }
          FALSE
        })
        if (adbc_ok) {
          state <- .new_result_state(rows_affected = 0L)
          state$fetched <- TRUE
          return(new("SnowflakeResult",
            connection = conn,
            statement  = statement,
            .resp_body = list(),
            .meta      = list(columns = data.frame(name = character(0),
                                                   type = character(0)),
                              num_rows = 0L, num_partitions = 0L,
                              statement_handle = ""),
            .state     = state
          ))
        }
      }
    }

    bindings <- if (!is.null(params)) .params_to_bindings(params) else NULL
    resp <- sf_api_submit(conn, statement, bindings = bindings)
    meta <- sf_parse_metadata(resp)
    rows <- .extract_rows_affected(resp)

    new("SnowflakeResult",
      connection = conn,
      statement  = statement,
      .resp_body = resp,
      .meta      = meta,
      .state     = .new_result_state(rows_affected = rows)
    )
  }
)

#' @rdname SnowflakeConnection-class
#' @export
setMethod("dbGetQuery", signature("SnowflakeConnection", "character"),
  function(conn, statement, params = NULL, ...) {
    .check_valid(conn)

    # ADBC fast path for parameterless SELECT queries
    backend <- getOption("skiLift.backend", "auto")
    if (is.null(params) && backend %in% c("adbc", "auto")) {
      adbc <- .ensure_adbc(conn)
      if (!is.null(adbc)) {
        adbc_result <- tryCatch(
          .adbc_get_query(adbc, statement),
          error = function(e) {
            if (!identical(backend, "auto")) {
              cli_warn(c(
                "!" = "ADBC query failed, falling back to REST API.",
                "i" = conditionMessage(e)
              ))
            }
            NULL
          }
        )
        if (!is.null(adbc_result)) return(adbc_result)
      }
    }

    # REST API v2 path
    bindings <- if (!is.null(params)) .params_to_bindings(params) else NULL
    resp <- sf_api_submit(conn, statement, bindings = bindings)
    parsed <- sf_parse_response(resp)
    meta <- parsed$meta

    if (meta$num_partitions > 1L) {
      remaining_indices <- seq.int(1L, meta$num_partitions - 1L)

      use_parallel <- isTRUE(getOption("skiLift.parallel_fetch", TRUE)) &&
                      length(remaining_indices) > 1L

      if (use_parallel) {
        rest <- sf_fetch_partitions_parallel(
          conn, meta$statement_handle, remaining_indices, meta
        )
      } else {
        rest <- .fetch_partitions_sequential(
          conn, meta$statement_handle, remaining_indices, meta
        )
      }

      parts <- c(list(parsed$data), rest)
      return(do.call(rbind, parts))
    }

    parsed$data
  }
)

#' @rdname SnowflakeConnection-class
#' @export
setMethod("dbExecute", signature("SnowflakeConnection", "character"),
  function(conn, statement, params = NULL, ...) {
    .check_valid(conn)

    # ADBC fast path for parameterless statements
    backend <- getOption("skiLift.backend", "auto")
    if (is.null(params) && backend %in% c("adbc", "auto")) {
      adbc <- .ensure_adbc(conn)
      if (!is.null(adbc)) {
        result <- tryCatch(
          .adbc_execute_sql(adbc, statement),
          error = function(e) {
            if (!identical(backend, "auto")) {
              cli_warn(c(
                "!" = "ADBC execute failed, falling back to REST API.",
                "i" = conditionMessage(e)
              ))
            }
            NULL
          }
        )
        if (!is.null(result)) return(result)
      }
    }

    bindings <- if (!is.null(params)) .params_to_bindings(params) else NULL
    resp <- sf_api_submit(conn, statement, bindings = bindings)
    .extract_rows_affected(resp)
  }
)

# ---------------------------------------------------------------------------
# DBI Arrow methods
#
# Priority order for Arrow results:
# 1. ADBC (when installed) -- true Arrow-native via the Go driver.
#    In Workspace: connects via SNOWFLAKE_HOST with SPCS OAuth.
#    Outside Workspace: connects via public endpoint with PAT/JWT.
# 2. Internal GS protocol (when a session is active and
#    use_native_arrow = TRUE) -- Arrow IPC from the server.  Does NOT work
#    in Workspace (requires a GS session, not available via REST/ADBC).
# 3. REST API v2 JSON with client-side Arrow conversion (universal fallback).
#    Note: SQL API v2 Arrow result format is gated internally and NOT GA.
# ---------------------------------------------------------------------------

#' @rdname SnowflakeConnection-class
#' @export
setMethod("dbGetQueryArrow", signature("SnowflakeConnection", "character"),
  function(conn, statement, params = NULL, ...) {
    rlang::check_installed("nanoarrow", reason = "for Arrow result format")
    .check_valid(conn)

    # ADBC Arrow path (preferred -- true server-side Arrow)
    backend <- getOption("skiLift.backend", "auto")
    if (is.null(params) && backend %in% c("adbc", "auto")) {
      adbc <- .ensure_adbc(conn)
      if (!is.null(adbc)) {
        adbc_stream <- tryCatch(
          .adbc_get_query_arrow(adbc, statement),
          error = function(e) NULL
        )
        if (!is.null(adbc_stream)) return(adbc_stream)
      }
    }

    bindings <- if (!is.null(params)) .params_to_bindings(params) else NULL

    # Native Arrow path via internal GS protocol
    if (.can_use_native_arrow(conn)) {
      resp <- tryCatch(
        sf_internal_submit_arrow(conn, statement, bindings = bindings),
        error = function(e) NULL
      )
      if (!is.null(resp)) {
        return(sf_fetch_all_native_arrow(conn, resp))
      }
    }

    # REST API v2 JSON → client-side Arrow conversion
    resp <- sf_api_submit(conn, statement, bindings = bindings)
    meta <- sf_parse_metadata(resp)
    sf_fetch_all_as_arrow_stream(conn, resp, meta)
  }
)

#' @rdname SnowflakeConnection-class
#' @export
setMethod("dbSendQueryArrow", signature("SnowflakeConnection", "character"),
  function(conn, statement, params = NULL, ...) {
    rlang::check_installed("nanoarrow", reason = "for Arrow result format")
    .check_valid(conn)
    bindings <- if (!is.null(params)) .params_to_bindings(params) else NULL
    resp <- sf_api_submit(conn, statement, bindings = bindings)
    meta <- sf_parse_metadata(resp)

    state <- .new_result_state(rows_affected = -1L)
    state$use_arrow <- TRUE

    new("SnowflakeResult",
      connection = conn,
      statement  = statement,
      .resp_body = resp,
      .meta      = meta,
      .state     = state
    )
  }
)

# ---------------------------------------------------------------------------
# Table operations
# ---------------------------------------------------------------------------

#' @rdname SnowflakeConnection-class
#' @export
setMethod("dbListTables", "SnowflakeConnection", function(conn, ...) {
  .check_valid(conn)
  df <- dbGetQuery(conn, "SHOW TABLES IN SCHEMA")
  if (nrow(df) == 0L) return(character(0))

  name_col <- which(tolower(names(df)) == "name")
  if (length(name_col) == 0L) return(character(0))

  df[[name_col]]
})

#' @rdname SnowflakeConnection-class
#' @param name Table name (character).
#' @export
setMethod("dbExistsTable", signature("SnowflakeConnection", "character"),
  function(conn, name, ...) {
    .check_valid(conn)
    tolower(.maybe_upcase(name)) %in% tolower(dbListTables(conn))
  }
)

#' @rdname SnowflakeConnection-class
#' @export
setMethod("dbListFields", signature("SnowflakeConnection", "character"),
  function(conn, name, ...) {
    .check_valid(conn)
    id <- dbQuoteIdentifier(conn, .maybe_upcase(name))
    df <- dbGetQuery(conn, paste0("SHOW COLUMNS IN TABLE ", id))
    if (nrow(df) == 0L) return(character(0))

    col_col <- which(tolower(names(df)) == "column_name")
    if (length(col_col) == 0L) return(character(0))

    df[[col_col]]
  }
)

#' @rdname SnowflakeConnection-class
#' @param prefix An `Id` object indicating the hierarchy level to list
#'   (NULL for databases, 1-component for schemas, 2-component for tables).
#' @export
setMethod("dbListObjects", signature("SnowflakeConnection"),
  function(conn, prefix = NULL, ...) {
    .check_valid(conn)

    if (is.null(prefix)) {
      return(.list_databases(conn))
    }

    parts <- prefix@name
    if (length(parts) == 1L) {
      return(.list_schemas(conn, parts[[1L]]))
    }
    if (length(parts) == 2L) {
      return(.list_tables_in_schema(conn, parts[[1L]], parts[[2L]]))
    }

    data.frame(table = I(list()), is_prefix = logical(0))
  }
)

.list_databases <- function(conn) {
  df <- dbGetQuery(conn, "SHOW DATABASES")
  if (nrow(df) == 0L) {
    return(data.frame(table = I(list()), is_prefix = logical(0)))
  }
  name_col <- which(tolower(names(df)) == "name")
  if (length(name_col) == 0L) {
    return(data.frame(table = I(list()), is_prefix = logical(0)))
  }
  dbs <- df[[name_col]]
  data.frame(
    table = I(lapply(dbs, function(d) Id(catalog = d))),
    is_prefix = rep(TRUE, length(dbs))
  )
}

.list_schemas <- function(conn, database) {
  qdb <- dbQuoteIdentifier(conn, database)
  df <- dbGetQuery(conn, paste0("SHOW SCHEMAS IN DATABASE ", qdb))
  if (nrow(df) == 0L) {
    return(data.frame(table = I(list()), is_prefix = logical(0)))
  }
  name_col <- which(tolower(names(df)) == "name")
  if (length(name_col) == 0L) {
    return(data.frame(table = I(list()), is_prefix = logical(0)))
  }
  schemas <- df[[name_col]]
  data.frame(
    table = I(lapply(schemas, function(s) Id(catalog = database, schema = s))),
    is_prefix = rep(TRUE, length(schemas))
  )
}

.list_tables_in_schema <- function(conn, database, schema) {
  qdb <- dbQuoteIdentifier(conn, database)
  qsch <- dbQuoteIdentifier(conn, schema)
  sql <- paste0("SHOW TABLES IN SCHEMA ", qdb, ".", qsch)
  df <- dbGetQuery(conn, sql)
  if (nrow(df) == 0L) {
    return(data.frame(table = I(list()), is_prefix = logical(0)))
  }
  name_col <- which(tolower(names(df)) == "name")
  if (length(name_col) == 0L) {
    return(data.frame(table = I(list()), is_prefix = logical(0)))
  }
  tables <- df[[name_col]]
  data.frame(
    table = I(lapply(tables, function(t) {
      Id(catalog = database, schema = schema, table = t)
    })),
    is_prefix = rep(FALSE, length(tables))
  )
}

#' @rdname SnowflakeConnection-class
#' @export
setMethod("dbReadTable", signature("SnowflakeConnection", "character"),
  function(conn, name, ...) {
    .check_valid(conn)
    id <- dbQuoteIdentifier(conn, .maybe_upcase(name))
    dbGetQuery(conn, paste0("SELECT * FROM ", id))
  }
)

#' @rdname SnowflakeConnection-class
#' @param value A data.frame.
#' @param overwrite Logical. Drop existing table first?
#' @param append Logical. Append to existing table?
#' @param row.names Ignored (always FALSE for Snowflake).
#' @export
setMethod("dbWriteTable",
  signature("SnowflakeConnection", "character", "data.frame"),
  function(conn, name, value, overwrite = FALSE, append = FALSE,
           row.names = FALSE, ...) {
    .check_valid(conn)
    name <- .maybe_upcase(name)
    names(value) <- .maybe_upcase(names(value))
    id <- dbQuoteIdentifier(conn, name)
    exists <- dbExistsTable(conn, name)

    if (exists && !overwrite && !append) {
      cli_abort("Table {.val {name}} already exists. Use {.arg overwrite} or {.arg append}.")
    }

    if (overwrite) {
      dbExecute(conn, paste0("DROP TABLE IF EXISTS ", id))
      dbCreateTable(conn, name, value)
    } else if (!exists) {
      dbCreateTable(conn, name, value)
    }

    if (nrow(value) > 0L) {
      .insert_data(conn, id, value)
    }

    invisible(TRUE)
  }
)

#' @rdname SnowflakeConnection-class
#' @param fields A data.frame or named character vector of column types.
#' @param temporary Logical. Create a temporary table?
#' @export
setMethod("dbCreateTable",
  signature("SnowflakeConnection", "character"),
  function(conn, name, fields, ..., row.names = NULL, temporary = FALSE) {
    .check_valid(conn)
    id <- dbQuoteIdentifier(conn, .maybe_upcase(name))

    if (is.data.frame(fields)) {
      col_types <- vapply(fields, r_to_sf_type, character(1))
      col_names <- .maybe_upcase(names(fields))
    } else if (is.character(fields)) {
      col_names <- .maybe_upcase(names(fields))
      col_types <- unname(fields)
    } else {
      cli_abort("{.arg fields} must be a data.frame or a named character vector.")
    }

    col_defs <- paste0(dbQuoteIdentifier(conn, col_names), " ", col_types)
    tmp <- if (temporary) "TEMPORARY " else ""
    ddl <- paste0("CREATE ", tmp, "TABLE ", id, " (\n  ",
                   paste(col_defs, collapse = ",\n  "), "\n)")
    dbExecute(conn, ddl)
    invisible(TRUE)
  }
)

#' @rdname SnowflakeConnection-class
#' @export
setMethod("dbAppendTable",
  signature("SnowflakeConnection", "character"),
  function(conn, name, value, ..., row.names = NULL) {
    .check_valid(conn)
    name <- .maybe_upcase(name)
    names(value) <- .maybe_upcase(names(value))
    id <- dbQuoteIdentifier(conn, name)
    if (nrow(value) == 0L) return(invisible(0L))
    .insert_data(conn, id, value)
    invisible(nrow(value))
  }
)

#' @rdname SnowflakeConnection-class
#' @export
setMethod("dbRemoveTable", signature("SnowflakeConnection", "character"),
  function(conn, name, ...) {
    .check_valid(conn)
    id <- dbQuoteIdentifier(conn, .maybe_upcase(name))
    dbExecute(conn, paste0("DROP TABLE IF EXISTS ", id))
    invisible(TRUE)
  }
)

# ---------------------------------------------------------------------------
# Quoting
# ---------------------------------------------------------------------------

#' @rdname SnowflakeConnection-class
#' @param x Character to quote.
#' @export
setMethod("dbQuoteIdentifier", signature("SnowflakeConnection", "character"),
  function(conn, x, ...) {
    needs_quote <- !grepl('^".*"$', x)
    x[needs_quote] <- paste0('"', gsub('"', '""', x[needs_quote]), '"')
    SQL(x)
  }
)

#' @rdname SnowflakeConnection-class
#' @export
setMethod("dbQuoteString", signature("SnowflakeConnection", "character"),
  function(conn, x, ...) {
    is_na <- is.na(x)
    x <- gsub("'", "''", x)
    x <- paste0("'", x, "'")
    x[is_na] <- "NULL"
    SQL(x)
  }
)

#' @rdname SnowflakeConnection-class
#' @export
setMethod("dbQuoteString", signature("SnowflakeConnection", "SQL"),
  function(conn, x, ...) { x }
)

#' @rdname SnowflakeConnection-class
#' @export
setMethod("dbQuoteIdentifier", signature("SnowflakeConnection", "SQL"),
  function(conn, x, ...) { x }
)

#' @rdname SnowflakeConnection-class
#' @export
setMethod("dbUnquoteIdentifier", signature("SnowflakeConnection", "SQL"),
  function(conn, x, ...) {
    lapply(x, function(id_str) {
      id_str <- as.character(id_str)
      parts <- character(0)
      remaining <- id_str

      while (nzchar(remaining)) {
        remaining <- trimws(remaining)
        if (startsWith(remaining, '"')) {
          close_pos <- regexpr('"([^"]|"")*"', remaining)
          if (close_pos == -1L) {
            parts <- c(parts, remaining)
            break
          }
          len <- attr(close_pos, "match.length")
          quoted <- substr(remaining, 2L, len - 1L)
          quoted <- gsub('""', '"', quoted)
          parts <- c(parts, quoted)
          remaining <- substr(remaining, len + 1L, nchar(remaining))
          remaining <- sub("^\\.", "", remaining)
        } else {
          dot_pos <- regexpr("\\.", remaining)
          if (dot_pos == -1L) {
            parts <- c(parts, remaining)
            break
          }
          parts <- c(parts, substr(remaining, 1L, dot_pos - 1L))
          remaining <- substr(remaining, dot_pos + 1L, nchar(remaining))
        }
      }

      switch(as.character(length(parts)),
        "1" = Id(table = parts[1L]),
        "2" = Id(schema = parts[1L], table = parts[2L]),
        "3" = Id(catalog = parts[1L], schema = parts[2L], table = parts[3L]),
        Id(table = id_str)
      )
    })
  }
)

#' @rdname SnowflakeConnection-class
#' @export
setMethod("dbQuoteLiteral", signature("SnowflakeConnection"),
  function(conn, x, ...) {
    if (is.factor(x)) return(dbQuoteString(conn, as.character(x)))
    if (is.character(x)) return(dbQuoteString(conn, x))
    if (is.logical(x)) {
      x <- ifelse(x, "TRUE", "FALSE")
      x[is.na(x)] <- "NULL"
      return(SQL(x))
    }
    if (inherits(x, "Date")) {
      is_na <- is.na(x)
      out <- paste0("'", format(x, "%Y-%m-%d"), "'::DATE")
      out[is_na] <- "NULL"
      return(SQL(out))
    }
    if (inherits(x, "POSIXct")) {
      is_na <- is.na(x)
      out <- paste0("'", format(x, "%Y-%m-%d %H:%M:%OS3", tz = "UTC"), "'::TIMESTAMP_NTZ")
      out[is_na] <- "NULL"
      return(SQL(out))
    }
    if (is.integer(x)) {
      is_na <- is.na(x)
      out <- as.character(x)
      out[is_na] <- "NULL"
      return(SQL(out))
    }
    if (is.numeric(x)) {
      is_na <- is.na(x)
      out <- format(x, scientific = FALSE)
      out[is_na] <- "NULL"
      return(SQL(out))
    }
    if (is.raw(x)) {
      return(SQL(paste0("X'", paste(format(x, width = 2), collapse = ""), "'")))
    }
    dbQuoteString(conn, as.character(x))
  }
)

# ---------------------------------------------------------------------------
# Data type
# ---------------------------------------------------------------------------

#' @rdname SnowflakeConnection-class
#' @param obj An R object to map to a Snowflake SQL type.
#' @export
setMethod("dbDataType", "SnowflakeConnection", function(dbObj, obj, ...) {
  r_to_sf_type(obj)
})

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

.check_valid <- function(conn) {
  if (!conn@.state$valid) {
    cli_abort("Connection is no longer valid. Use {.fn dbConnect} to reconnect.")
  }
}

.extract_rows_affected <- function(resp) {
  stats <- resp$stats
  if (!is.null(stats)) {
    counts <- c(
      stats$numRowsInserted,
      stats$numRowsUpdated,
      stats$numRowsDeleted,
      stats$numRowsUnloaded
    )
    counts <- counts[!is.na(counts)]
    if (length(counts) > 0L) return(as.integer(sum(counts)))
  }
  0L
}

#' Convert R params to SQL API v2 bindings format
#' @noRd
.params_to_bindings <- function(params) {
  if (is.null(params) || length(params) == 0L) return(NULL)

  if (is.data.frame(params)) {
    params <- as.list(params)
  }

  bindings <- list()
  for (i in seq_along(params)) {
    nm <- as.character(i)
    val <- params[[i]]

    if (is.logical(val)) {
      bindings[[nm]] <- list(type = "BOOLEAN", value = as.character(val))
    } else if (is.integer(val)) {
      bindings[[nm]] <- list(type = "FIXED", value = as.character(val))
    } else if (is.numeric(val)) {
      bindings[[nm]] <- list(type = "REAL", value = as.character(val))
    } else {
      bindings[[nm]] <- list(type = "TEXT", value = as.character(val))
    }
  }
  bindings
}

#' Insert data.frame rows, routing to the best available backend
#'
#' Routing order for `"auto"` (default):
#' - ADBC bulk ingest when available AND cell count >= threshold.
#'   In Workspace, ADBC connects via SNOWFLAKE_HOST (no public endpoint).
#' - In Workspace: Snowpark `write_pandas` as fallback when ADBC absent.
#' - Otherwise: literal SQL INSERT (routed via ADBC if available, else REST).
#'
#' Explicit methods:
#' - `"snowpark"`: force Snowpark write_pandas; falls back to literal.
#' - `"adbc"`: force ADBC; falls back to literal if unavailable.
#' - `"literal"`: SQL string INSERT (uses ADBC when available, else REST).
#' - `"bind"`: DEPRECATED.
#' @noRd
.insert_data <- function(conn, table_id, df) {
  method <- getOption("skiLift.upload_method", "auto")
  cells <- nrow(df) * ncol(df)
  threshold <- as.integer(
    getOption("skiLift.bulk_write_threshold",
              getOption("skiLift.adbc_write_threshold", 50000L))
  )
  in_workspace <- nzchar(Sys.getenv("SNOWFLAKE_HOST", ""))

  # --- auto routing ---
  # In Workspace: ADBC via internal SPCS host is now fast (~7s for 50K rows)
  # and avoids the reticulate/Python dependency.  Snowpark write_pandas is
  # kept as a fallback when ADBC is not installed.
  if (identical(method, "auto") && cells >= threshold) {
    adbc <- .ensure_adbc(conn)
    if (!is.null(adbc)) {
      return(.insert_data_adbc(adbc, table_id, df))
    }
    if (in_workspace && .snowpark_write_available()) {
      return(.insert_data_snowpark(conn, table_id, df))
    }
  }

  # --- explicit snowpark ---
  if (identical(method, "snowpark")) {
    return(.insert_data_snowpark(conn, table_id, df))
  }

  # --- explicit adbc ---
  if (identical(method, "adbc")) {
    adbc <- .ensure_adbc(conn)
    if (!is.null(adbc)) {
      return(.insert_data_adbc(adbc, table_id, df))
    }
    cli_warn("ADBC backend unavailable, falling back to literal INSERT.")
  }

  # --- deprecated bind ---
  if (identical(method, "bind")) {
    cli_warn(c(
      "!" = "{.code upload_method = \"bind\"} is deprecated.",
      "i" = "SQL API v2 has no array-binding support; bind-parameter INSERT is slower than literal INSERT.",
      "i" = "Use {.code \"auto\"} (default) or {.code \"literal\"} instead."
    ))
    return(.insert_data_bind(conn, table_id, df))
  }

  .insert_data_literal(conn, table_id, df)
}


#' Thin wrapper for ADBC bulk ingest called from .insert_data
#'
#' Strips DBI quoting from the table identifier before handing off to
#' the ADBC backend, which expects an unquoted table name.
#' @noRd
.insert_data_adbc <- function(adbc, table_id, df) {
  plain_name <- gsub('"', "", table_id)
  .adbc_write_table(adbc, plain_name, df, mode = "append")
}

#' Legacy INSERT path: inline SQL string literals
#'
#' Uses ADBC when available (avoids REST API on public endpoint).
#' @noRd
.insert_data_literal <- function(conn, table_id, df) {
  batch_size <- getOption("skiLift.insert_batch_size", 5000L)
  batch_size <- as.integer(batch_size)
  n <- nrow(df)
  ncols <- ncol(df)

  adbc <- .ensure_adbc(conn)

  col_clause <- paste0(
    " (",
    paste(vapply(names(df), function(nm) {
      dbQuoteIdentifier(conn, nm)
    }, character(1)), collapse = ", "),
    ")"
  )

  use_progress <- n > batch_size && requireNamespace("cli", quietly = TRUE)
  if (use_progress) {
    pb <- cli::cli_progress_bar(
      total   = n,
      format  = "Inserting rows {cli::pb_current}/{cli::pb_total} {cli::pb_bar} {cli::pb_percent}"
    )
  }

  for (start in seq(1L, n, by = batch_size)) {
    end <- min(start + batch_size - 1L, n)
    rows <- vapply(start:end, function(i) {
      vals <- vapply(seq_len(ncols), function(j) {
        .format_value(df[[j]][i])
      }, character(1))
      paste0("(", paste(vals, collapse = ", "), ")")
    }, character(1))

    sql <- paste0(
      "INSERT INTO ", table_id, col_clause, " VALUES\n",
      paste(rows, collapse = ",\n")
    )

    if (!is.null(adbc)) {
      .adbc_execute_sql(adbc, sql)
    } else {
      sf_api_submit(conn, sql)
    }

    if (use_progress) cli::cli_progress_update(set = end, id = pb)
  }

  if (use_progress) cli::cli_progress_done(id = pb)
}

.format_value <- function(x) {
  if (is.na(x)) return("NULL")
  if (is.logical(x)) return(if (x) "TRUE" else "FALSE")
  if (is.numeric(x)) return(as.character(x))
  if (inherits(x, "Date")) return(paste0("'", format(x, "%Y-%m-%d"), "'"))
  if (inherits(x, "POSIXct")) return(paste0("'", format(x, "%Y-%m-%d %H:%M:%S"), "'"))
  paste0("'", gsub("'", "''", as.character(x)), "'")
}

#' @rdname SnowflakeConnection-class
#' @param con A [SnowflakeConnection-class] object.
#' @export
setMethod("sqlData", "SnowflakeConnection",
  function(con, value, row.names = FALSE, ...) {
    as.data.frame(
      lapply(value, function(col) {
        if (is.logical(col)) {
          ifelse(col, "TRUE", "FALSE")
        } else if (inherits(col, "Date")) {
          format(col, "%Y-%m-%d")
        } else if (inherits(col, "POSIXct")) {
          format(col, "%Y-%m-%d %H:%M:%OS3", tz = "UTC")
        } else {
          as.character(col)
        }
      }),
      stringsAsFactors = FALSE
    )
  }
)
