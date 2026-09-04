# Parallel Partition Fetching
# =============================================================================
# Fetches multiple JSON partitions concurrently.
#
# Strategy cascade (first success wins):
#   1. parLapply  -- socket cluster, TLS-safe, requires skiLift installed
#   2. mclapply   -- fork-based, no install needed, safe on Linux (OpenSSL)
#                    but fails on macOS (SecureTransport can't fork)
#   3. sequential -- guaranteed fallback
#
# In production (Workspace Notebooks) the package is installed, so (1) is used.
# During devtools development on Linux, (2) kicks in.  On macOS dev, (3).

#' Resolve the effective number of parallel workers
#'
#' When `skiLift.fetch_workers` is 0 or "auto" (the default), detects the
#' host's available CPU cores via `parallel::detectCores()` and reserves one
#' for the main R thread.  A positive integer is used as-is.  The result is
#' always clamped to `[1, n_tasks]`.
#'
#' Workspace Notebooks run inside SPCS containers whose vCPU count varies by
#' instance family (CPU_X64_XS = 1, CPU_X64_S = 3, CPU_X64_M = 6, etc.).
#' Auto-detection adapts to whatever the container exposes.
#'
#' @param n_tasks Number of tasks (partitions/chunks) to parallelise over.
#' @returns Integer number of workers to use.
#' @noRd
.resolve_n_workers <- function(n_tasks) {
  opt <- getOption("skiLift.fetch_workers", 0L)

  if (is.character(opt) && tolower(opt) == "auto") opt <- 0L
  opt <- suppressWarnings(as.integer(opt))
  if (is.na(opt)) opt <- 0L

  if (opt <= 0L) {
    detected <- tryCatch(
      parallel::detectCores(logical = FALSE),
      error = function(e) NA_integer_
    )
    if (is.na(detected) || detected < 1L) detected <- 2L
    opt <- max(1L, detected - 1L)
  }

  # Never more workers than tasks
  min(opt, n_tasks)
}

#' Fetch and parse multiple partitions in parallel
#'
#' @param con SnowflakeConnection.
#' @param handle Statement handle string.
#' @param partition_indices Integer vector of 0-based partition indices to fetch.
#' @param meta Metadata from sf_parse_metadata() (used for parsing).
#' @returns A list of data.frames, one per partition.
#' @noRd
sf_fetch_partitions_parallel <- function(con, handle, partition_indices, meta) {
  n_workers <- .resolve_n_workers(length(partition_indices))

  if (n_workers <= 1L) {
    return(.fetch_partitions_sequential(con, handle, partition_indices, meta))
  }

  # Cascade: parLapply -> mclapply -> sequential
  frames <- .try_fetch_cluster(con, handle, partition_indices, n_workers, meta)
  if (!is.null(frames)) return(frames)

  frames <- .try_fetch_mclapply(con, handle, partition_indices, n_workers, meta)
  if (!is.null(frames)) return(frames)

  cli_warn(c(
    "!" = "Parallel partition fetch unavailable, using sequential.",
    "i" = paste0("parLapply requires skiLift to be installed; ",
                 "mclapply failed (likely macOS TLS fork issue).")
  ))
  .fetch_partitions_sequential(con, handle, partition_indices, meta)
}

#' Parse a partition response using metadata from the initial response
#'
#' Partition GET responses may omit `resultSetMetaData.rowType`, so we
#' carry the column metadata from the first response rather than
#' re-deriving it from each partition.
#' @noRd
.parse_partition_data <- function(part_resp, meta) {
  raw_data <- part_resp$data
  if (is.null(raw_data) || length(raw_data) == 0L) {
    return(.empty_df_from_meta(meta))
  }
  .json_data_to_df(raw_data, meta)
}

#' Sequential partition fetch (baseline / fallback)
#' @noRd
.fetch_partitions_sequential <- function(con, handle, partition_indices, meta) {
  lapply(partition_indices, function(idx) {
    part_resp <- sf_api_fetch_partition(con, handle, idx)
    .parse_partition_data(part_resp, meta)
  })
}

#' Try socket-cluster parallel fetch (requires installed package)
#' @returns List of data.frames on success, NULL on failure.
#' @noRd
.try_fetch_cluster <- function(con, handle, partition_indices, n_workers, meta) {
  tryCatch({
    cl <- parallel::makeCluster(n_workers)
    on.exit(parallel::stopCluster(cl), add = TRUE)

    parallel::clusterExport(cl, c("con", "handle", "meta"), envir = environment())
    parallel::clusterEvalQ(cl, {
      loadNamespace("skiLift")
      loadNamespace("httr2")
    })

    parallel::parLapply(cl, partition_indices, function(idx) {
      part_resp <- skiLift:::sf_api_fetch_partition(con, handle, idx)
      skiLift:::.parse_partition_data(part_resp, meta)
    })
  }, error = function(e) NULL)
}

#' Try fork-based parallel fetch (Unix/Linux only, no install needed)
#' @returns List of data.frames on success, NULL on failure.
#' @noRd
.try_fetch_mclapply <- function(con, handle, partition_indices, n_workers, meta) {
  if (.Platform$OS.type == "windows") return(NULL)

  tryCatch({
    frames <- parallel::mclapply(partition_indices, function(idx) {
      part_resp <- sf_api_fetch_partition(con, handle, idx)
      .parse_partition_data(part_resp, meta)
    }, mc.cores = n_workers)

    # mclapply embeds errors as try-error objects rather than throwing
    errs <- vapply(frames, inherits, logical(1), "try-error")
    if (any(errs)) return(NULL)

    frames
  }, error = function(e) NULL)
}
