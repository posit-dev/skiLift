# ADBC Backend (Primary in Workspace, Optional Accelerator Outside)
# =============================================================================
# When adbcsnowflake and adbcdrivermanager are installed, skiLift routes
# ALL DBI operations through ADBC: reads, writes, DDL (dbExecute,
# dbCreateTable, dbRemoveTable), and metadata (dbListTables, dbListFields).
# This eliminates the need for the public endpoint in the EAI.
#
# The REST API v2 remains the fallback when ADBC packages are absent.
# In Workspace, a minted session token (Path B) can replace the public
# endpoint even without ADBC.
#
# Connectivity (updated Mar 2026 -- corrects earlier "blessed client" note):
# - ADBC wraps the Go Snowflake driver (gosnowflake).  Like all official
#   Snowflake drivers (JDBC, ODBC, Go, Python, .NET), it can authenticate
#   with the SPCS OAuth token at /snowflake/session/token **provided** the
#   connection targets the internal SPCS gateway (SNOWFLAKE_HOST) and the
#   token is passed as a connection property (not embedded in a URL).
# - In Workspace Notebooks (SPCS containers), ADBC connects to the
#   internal host with authenticator = "auth_oauth".  No PAT, network
#   policy, or public endpoint is required.
# - Outside Workspace, ADBC connects to <account>.snowflakecomputing.com
#   using whatever auth the user configured (PAT, JWT, etc.).
# - SQL API v2 does NOT support Arrow result format (gated behind an
#   internal flag, not GA).  ADBC is the only way to get Arrow-native
#   reads from R today without a GS session.
#
# The ADBC backend is lazy-initialised on first use and stored in the
# connection's mutable .state environment.

# ---------------------------------------------------------------------------
# Availability checks
# ---------------------------------------------------------------------------

#' Check whether the ADBC packages are installed
#' @noRd
.adbc_packages_available <- function() {
  requireNamespace("adbcsnowflake", quietly = TRUE) &&
    requireNamespace("adbcdrivermanager", quietly = TRUE)
}

#' Check whether a connection has a live ADBC backend
#' @noRd
.has_adbc <- function(conn) {
  adbc <- conn@.state$adbc
  !is.null(adbc) && !identical(adbc, "failed") && !is.null(adbc$con)
}

# ---------------------------------------------------------------------------
# Initialisation & lifecycle
# ---------------------------------------------------------------------------

#' Lazy-init the ADBC backend, returning the backend list or NULL
#'
#' On first call the backend is created and cached in conn@.state$adbc.
#' Subsequent calls return the cached value immediately.  A failed init
#' is cached as the string "failed" so we don't retry on every call.
#' @noRd
.ensure_adbc <- function(conn) {
  cached <- conn@.state$adbc
  if (!is.null(cached)) {
    if (identical(cached, "failed")) return(NULL)
    return(cached)
  }

  backend <- getOption("skiLift.backend", "auto")
  if (identical(backend, "rest")) return(NULL)

  t0 <- proc.time()
  adbc <- .init_adbc_backend(conn)
  elapsed <- (proc.time() - t0)[["elapsed"]]

  if (is.null(adbc)) {
    conn@.state$adbc <- "failed"
    return(NULL)
  }

  if (isTRUE(getOption("skiLift.verbose", FALSE))) {
    cli_inform(c("i" = "ADBC backend init took {.val {round(elapsed, 1)}}s."))
  }

  conn@.state$adbc <- adbc
  adbc
}

#' Create an ADBC database + connection using the same credentials as the
#' REST API v2 connection.
#' @noRd
.init_adbc_backend <- function(conn) {
  if (!.adbc_packages_available()) return(NULL)

  tryCatch({
    auth_info <- .adbc_auth_args(conn)

    args <- c(
      list(
        driver   = adbcsnowflake::adbcsnowflake(),
        username = conn@user,
        `adbc.snowflake.sql.account`   = conn@account,
        `adbc.snowflake.sql.db`        = conn@database,
        `adbc.snowflake.sql.schema`    = conn@schema,
        `adbc.snowflake.sql.warehouse` = conn@warehouse
      ),
      auth_info
    )

    # In Workspace, connect via the internal SPCS gateway for lower latency
    # and zero-config auth.  Outside Workspace, use the public endpoint.
    spcs_host <- Sys.getenv("SNOWFLAKE_HOST", "")
    if (nzchar(spcs_host)) {
      args[["adbc.snowflake.sql.uri.host"]] <- spcs_host
    } else {
      args[["adbc.snowflake.sql.uri.host"]] <- paste0(
        conn@account, ".snowflakecomputing.com"
      )
    }

    if (nzchar(conn@role)) {
      args[["adbc.snowflake.sql.role"]] <- conn@role
    }

    db <- do.call(adbcdrivermanager::adbc_database_init, args)
    adbc_con <- adbcdrivermanager::adbc_connection_init(db)

    if (isTRUE(getOption("skiLift.verbose", FALSE))) {
      if (nzchar(spcs_host)) {
        cli_inform(c(
          "i" = "ADBC backend initialised (Workspace: SPCS OAuth via internal host).",
          "i" = "Host: {.val {spcs_host}}"
        ))
      } else {
        cli_inform(c("i" = "ADBC backend initialised."))
      }
    }

    list(db = db, con = adbc_con)
  }, error = function(e) {
    if (!identical(getOption("skiLift.backend", "auto"), "auto")) {
      in_workspace <- nzchar(Sys.getenv("SNOWFLAKE_HOST", ""))
      hints <- conditionMessage(e)
      if (in_workspace && grepl("auth|token|oauth|network", hints, ignore.case = TRUE)) {
        cli_warn(c(
          "!" = "ADBC backend initialisation failed inside Workspace.",
          "i" = hints,
          "i" = "Verify that /snowflake/session/token exists and SNOWFLAKE_HOST is set.",
          "i" = "Fallback: set SNOWFLAKE_PAT for PAT auth to the public endpoint."
        ))
      } else {
        cli_warn(c(
          "!" = "ADBC backend initialisation failed.",
          "i" = hints
        ))
      }
    }
    NULL
  })
}

#' Map skiLift auth to ADBC driver arguments
#' @returns Named list of ADBC auth options.
#' @noRd
.adbc_auth_args <- function(conn) {
  auth <- conn@.auth

  if (auth$type == "oauth") {
    return(list(
      `adbc.snowflake.sql.auth_type` = "auth_oauth",
      `adbc.snowflake.sql.client_option.auth_token` = auth$token
    ))
  }

  if (auth$type %in% c("pat", "token")) {
    return(list(
      `adbc.snowflake.sql.auth_type` = "auth_pat",
      `adbc.snowflake.sql.client_option.auth_token` = auth$token
    ))
  }

  if (auth$type == "jwt") {
    key_path <- auth$private_key_path %||% ""
    if (nzchar(key_path) && file.exists(key_path)) {
      return(list(
        `adbc.snowflake.sql.auth_type` = "auth_jwt",
        `adbc.snowflake.sql.client_option.jwt_private_key` = normalizePath(key_path)
      ))
    }
    return(list(
      `adbc.snowflake.sql.auth_type` = "auth_jwt",
      `adbc.snowflake.sql.client_option.auth_token` = auth$token
    ))
  }

  list()
}

# ---------------------------------------------------------------------------
# Read helpers
# ---------------------------------------------------------------------------

#' Execute a SQL query via ADBC and return a data.frame
#' @noRd
.adbc_get_query <- function(adbc, statement) {
  result <- adbcdrivermanager::read_adbc(adbc$con, statement)
  as.data.frame(result)
}

#' Execute a SQL query via ADBC and return a nanoarrow array stream
#' @noRd
.adbc_get_query_arrow <- function(adbc, statement) {
  stmt <- adbcdrivermanager::adbc_statement_init(adbc$con)
  adbcdrivermanager::adbc_statement_set_sql_query(stmt, statement)
  stream <- nanoarrow::nanoarrow_allocate_array_stream()
  adbcdrivermanager::adbc_statement_execute_query(stmt, stream)
  stream
}

# ---------------------------------------------------------------------------
# Write helpers
# ---------------------------------------------------------------------------

#' Bulk ingest a data.frame via ADBC (PUT + COPY INTO under the hood)
#'
#' Uses the high-level `write_adbc()` which handles statement lifecycle
#' and is compatible across adbcdrivermanager versions.
#' For schema-qualified names, switches the ADBC session schema first.
#'
#' @param adbc The ADBC backend list (with $db and $con).
#' @param table_name Unquoted table name, possibly schema-qualified (e.g. "SCHEMA.TABLE").
#' @param df data.frame to ingest.
#' @param mode ADBC ingest mode: "default", "create", or "append".
#' @noRd
.adbc_write_table <- function(adbc, table_name, df, mode = "append") {
  verbose <- isTRUE(getOption("skiLift.verbose", FALSE))
  parts <- strsplit(table_name, ".", fixed = TRUE)[[1L]]

  if (length(parts) >= 2L) {
    schema_part <- parts[length(parts) - 1L]
    table_part <- parts[length(parts)]

    if (verbose) t_schema <- proc.time()
    prev_schema <- tryCatch(
      .adbc_get_query(adbc, "SELECT CURRENT_SCHEMA()")[[1L]],
      error = function(e) NULL
    )

    .adbc_execute_sql(adbc, paste("USE SCHEMA", schema_part))
    if (verbose) {
      cli_inform(c("i" = "ADBC schema switch: {.val {round((proc.time() - t_schema)[['elapsed']], 1)}}s"))
    }
    on.exit({
      if (!is.null(prev_schema) && nzchar(prev_schema)) {
        tryCatch(
          .adbc_execute_sql(adbc, paste("USE SCHEMA", prev_schema)),
          error = function(e) NULL
        )
      }
    }, add = TRUE)
  } else {
    table_part <- table_name
  }

  if (verbose) {
    cli_inform(c(
      "i" = "ADBC write_adbc: {nrow(df)} rows x {ncol(df)} cols to {.val {table_part}} (mode={.val {mode}})"
    ))
    t_write <- proc.time()
  }

  adbcdrivermanager::write_adbc(df, adbc$con, table_part, mode = mode)

  if (verbose) {
    cli_inform(c("i" = "ADBC write_adbc completed in {.val {round((proc.time() - t_write)[['elapsed']], 1)}}s"))
  }

  invisible(TRUE)
}

#' Execute a SQL statement on the ADBC connection
#'
#' Returns 0L so callers like dbExecute can use the value directly.
#' @noRd
.adbc_execute_sql <- function(adbc, sql) {
  stmt <- adbcdrivermanager::adbc_statement_init(adbc$con)
  on.exit(tryCatch(
    adbcdrivermanager::adbc_statement_release(stmt),
    error = function(e) NULL
  ))
  adbcdrivermanager::adbc_statement_set_sql_query(stmt, sql)
  adbcdrivermanager::adbc_statement_execute_query(stmt)
  0L
}

# ---------------------------------------------------------------------------
# Cleanup
# ---------------------------------------------------------------------------

#' Release ADBC connection and database resources
#' @noRd
.adbc_cleanup <- function(adbc) {
  if (is.null(adbc)) return(invisible())
  tryCatch({
    if (!is.null(adbc$con)) {
      adbcdrivermanager::adbc_connection_release(adbc$con)
    }
    if (!is.null(adbc$db)) {
      adbcdrivermanager::adbc_database_release(adbc$db)
    }
  }, error = function(e) NULL)
  invisible()
}
