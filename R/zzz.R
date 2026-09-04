.pkg_env <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  op <- options()

  # Bulk write threshold: in "auto" mode, writes above this cell count

  # (rows * cols) are routed to the fast bulk backend:
  #   - Workspace: Snowpark write_pandas (internal SPCS path, ~2.5s/50K rows)
  #   - External:  ADBC PUT+COPY INTO (public endpoint, ~5-10s/50K rows)
  # Below threshold, literal SQL INSERT is used (fast for small data).
  # Workspace threshold is higher because even the fast Snowpark path has
  # ~2s fixed overhead vs ~1s for literal INSERT on small data.
  in_workspace <- nzchar(Sys.getenv("SNOWFLAKE_HOST", ""))
  bulk_threshold <- if (in_workspace) 200000L else 50000L

  op_rsf <- list(
    skiLift.timeout              = 600L,
    skiLift.retry_max            = 3L,
    skiLift.result_format        = "json",
    skiLift.insert_batch_size    = 16384L,
    skiLift.upload_method        = "auto",
    skiLift.identifier_case      = "upper",
    skiLift.use_simdjson         = TRUE,
    skiLift.parallel_fetch       = TRUE,
    skiLift.fetch_workers        = 0L,
    skiLift.use_session          = FALSE,
    skiLift.use_native_arrow     = FALSE,
    skiLift.verbose              = FALSE,
    skiLift.backend              = "auto",
    skiLift.bulk_write_threshold = bulk_threshold,
    skiLift.adbc_write_threshold = bulk_threshold
  )
  toset <- !(names(op_rsf) %in% names(op))
  if (any(toset)) options(op_rsf[toset])

  .register_dbplyr_methods()

  invisible()
}

#' Apply identifier case policy
#'
#' When `skiLift.identifier_case` is `"upper"` (the default), identifiers
#' are uppercased before quoting, matching Snowflake's default behavior for
#' unquoted identifiers and the behavior of the ODBC driver.  When set to
#' `"preserve"`, identifiers retain their original case.
#' @param x Character vector of identifier names.
#' @returns Character vector, possibly uppercased.
#' @noRd
.maybe_upcase <- function(x) {
  if (identical(getOption("skiLift.identifier_case", "upper"), "upper")) {
    toupper(x)
  } else {
    x
  }
}
