# Snowpark write_pandas Backend (Workspace Fallback)
# =============================================================================
# In Workspace Notebooks (SPCS), ADBC is now the preferred write path.  With
# the internal SPCS host + OAuth, ADBC achieves ~7s for 50K-row writes --
# competitive with Snowpark write_pandas (~2.5s) and without the reticulate
# cross-language dependency.
#
# This module is retained as a fallback for when ADBC packages are not
# installed (they are not pre-installed in the Workspace container today).
# It routes bulk writes through Python's write_pandas via reticulate, using
# the active Workspace kernel's Snowpark session.  Outside Workspace, it can
# create a session from skiLift connection credentials.
#
# Benchmarks (50K rows, Mar 2026):
#   ADBC via internal host:   ~7s  (preferred)
#   Snowpark write_pandas:    ~2.5s (fastest, but requires reticulate)
#   Literal INSERT via REST:  ~45s  (universal fallback)
#
# The rpy2 in-memory R->Pandas conversion adds ~0.2s for 50K rows, which is
# negligible compared to the write cost.

# ---------------------------------------------------------------------------
# Availability checks
# ---------------------------------------------------------------------------

#' Check whether reticulate is loadable (thin wrapper for test mocking)
#' @noRd
.has_reticulate <- function() {
  requireNamespace("reticulate", quietly = TRUE)
}

#' Check whether the Snowpark write path is available
#' @returns TRUE if reticulate and snowflake.snowpark are importable.
#' @noRd
.snowpark_write_available <- function() {
  if (!.has_reticulate()) return(FALSE)
  tryCatch({
    reticulate::import("snowflake.snowpark", delay_load = FALSE)
    reticulate::import("pandas", delay_load = FALSE)
    TRUE
  }, error = function(e) FALSE)
}

# ---------------------------------------------------------------------------
# Session management
# ---------------------------------------------------------------------------

#' Get or create a cached Snowpark Python session
#'
#' In Workspace: uses get_active_session() (zero-config, blessed path).
#' Outside Workspace: creates a Session from conn credentials (requires
#' snowflake-snowpark-python in the user's Python env).
#'
#' The session is cached in conn@.state$snowpark_session.  A failed init
#' is cached as "failed" to avoid repeated retries.
#' @noRd
.ensure_snowpark_session <- function(conn) {
  cached <- conn@.state$snowpark_session
  if (!is.null(cached)) {
    if (identical(cached, "failed")) return(NULL)
    return(cached)
  }

  if (!.has_reticulate()) {
    conn@.state$snowpark_session <- "failed"
    return(NULL)
  }

  session <- tryCatch(
    .get_or_create_snowpark_session(conn),
    error = function(e) {
      cli_warn(c(
        "!" = "Snowpark session init failed.",
        "i" = conditionMessage(e)
      ))
      NULL
    }
  )

  if (is.null(session)) {
    conn@.state$snowpark_session <- "failed"
    return(NULL)
  }

  if (isTRUE(getOption("skiLift.verbose", FALSE))) {
    in_ws <- nzchar(Sys.getenv("SNOWFLAKE_HOST", ""))
    if (in_ws) {
      cli_inform(c("i" = "Snowpark session acquired (Workspace active session, internal SPCS path)."))
    } else {
      cli_inform(c("i" = "Snowpark session created from skiLift connection credentials."))
    }
  }

  conn@.state$snowpark_session <- session
  session
}

#' Obtain a Snowpark session: Workspace active session or new from credentials
#' @noRd
.get_or_create_snowpark_session <- function(conn) {
  in_workspace <- nzchar(Sys.getenv("SNOWFLAKE_HOST", ""))

  if (in_workspace) {
    ctx <- reticulate::import("snowflake.snowpark.context")
    return(ctx$get_active_session())
  }

  .snowpark_session_from_conn(conn)
}

#' Create a Snowpark Session from skiLift connection credentials
#'
#' Used outside Workspace when the user explicitly opts into
#' upload_method = "snowpark".  Requires snowflake-snowpark-python
#' and appropriate Python environment.
#' @noRd
.snowpark_session_from_conn <- function(conn) {
  snowpark <- reticulate::import("snowflake.snowpark")
  crypto_serial <- reticulate::import("cryptography.hazmat.primitives.serialization")

  configs <- list(
    account   = conn@account,
    user      = conn@user,
    database  = conn@database,
    schema    = conn@schema,
    warehouse = conn@warehouse,
    role      = conn@role
  )

  auth <- conn@.auth
  if (auth$type == "jwt" && !is.null(auth$private_key_path) &&
      nzchar(auth$private_key_path) && file.exists(auth$private_key_path)) {
    key_bytes <- readBin(auth$private_key_path, "raw",
                         file.info(auth$private_key_path)$size)
    py_key <- crypto_serial$load_pem_private_key(key_bytes, password = NULL)
    der_bytes <- py_key$private_bytes(
      encoding    = crypto_serial$Encoding$DER,
      format      = crypto_serial$PrivateFormat$PKCS8,
      encryption_algorithm = crypto_serial$NoEncryption()
    )
    configs[["private_key"]] <- der_bytes
  } else if (auth$type %in% c("pat", "token") && nzchar(auth$token)) {
    configs[["authenticator"]] <- "oauth"
    configs[["token"]] <- auth$token
  } else {
    cli_abort(c(
      "Cannot create external Snowpark session.",
      "i" = "Key-pair (JWT) or PAT authentication required.",
      "i" = "Set {.arg private_key_path} in dbConnect() or {.envvar SNOWFLAKE_PAT}."
    ))
  }

  snowpark$Session$builder$configs(configs)$create()
}

# ---------------------------------------------------------------------------
# Write helper
# ---------------------------------------------------------------------------

#' Bulk write a data.frame via Snowpark write_pandas
#'
#' Converts the R data.frame to a Pandas DataFrame via reticulate (in-memory),
#' then calls session.write_pandas() which uses PUT + COPY INTO internally.
#'
#' @param conn SnowflakeConnection (for session lookup and table context).
#' @param table_id Quoted table identifier string (DBI-style).
#' @param df data.frame to write.
#' @noRd
.insert_data_snowpark <- function(conn, table_id, df) {
  session <- .ensure_snowpark_session(conn)
  if (is.null(session)) {
    cli_warn("Snowpark session unavailable, falling back to literal INSERT.")
    return(.insert_data_literal(conn, table_id, df))
  }

  verbose <- isTRUE(getOption("skiLift.verbose", FALSE))
  plain_name <- gsub('"', "", table_id)

  if (verbose) {
    cli_inform(c("i" = "Snowpark write_pandas: {nrow(df)} rows x {ncol(df)} cols to {.val {plain_name}}"))
    t0 <- proc.time()
  }

  pdf <- reticulate::r_to_py(df)

  if (verbose) {
    conv_time <- (proc.time() - t0)[["elapsed"]]
    cli_inform(c("i" = "R->Pandas conversion: {.val {round(conv_time, 2)}}s"))
  }

  tryCatch({
    if (verbose) t_write <- proc.time()

    session$write_pandas(
      pdf, plain_name,
      auto_create_table = FALSE,
      overwrite         = FALSE,
      quote_identifiers = TRUE
    )

    if (verbose) {
      write_time <- (proc.time() - t_write)[["elapsed"]]
      cli_inform(c("i" = "write_pandas completed in {.val {round(write_time, 1)}}s"))
    }
  }, error = function(e) {
    cli_warn(c(
      "!" = "Snowpark write_pandas failed, falling back to literal INSERT.",
      "i" = conditionMessage(e)
    ))
    .insert_data_literal(conn, table_id, df)
  })

  invisible(TRUE)
}
