# Arrow Result Support (via nanoarrow)
# =============================================================================
# Two modes:
#
# 1. Client-side conversion (SQL API v2): Fetch JSON data through the normal
#    path, then convert R data.frames to nanoarrow array streams.  This adds
#    overhead compared to dbGetQuery and exists for DBI interface compatibility.
#
# 2. Native Arrow transport (session + internal protocol): When a session is
#    active and skiLift.use_native_arrow is TRUE, queries are submitted via
#    the internal protocol requesting Arrow IPC format.  Results arrive as
#    base64-encoded Arrow IPC (first partition) and pre-signed cloud URLs
#    (subsequent partitions).  This provides 5-10x improvement for large results.
#
# All nanoarrow calls are guarded with rlang::check_installed() since
# nanoarrow is in Suggests.


#' Fetch all JSON partitions and return as a nanoarrow_array_stream
#'
#' @param con SnowflakeConnection.
#' @param resp_body Initial response body from sf_api_submit (contains first
#'   partition inline).
#' @param meta Metadata from sf_parse_metadata().
#' @returns A nanoarrow_array_stream.
#' @noRd
sf_fetch_all_as_arrow_stream <- function(con, resp_body, meta) {
  rlang::check_installed("nanoarrow", reason = "for Arrow result format")

  handle  <- meta$statement_handle
  n_parts <- max(meta$num_partitions, 1L)

  parsed <- sf_parse_response(resp_body)
  first_frame <- parsed$data

  if (n_parts <= 1L) {
    combined <- first_frame
  } else {
    remaining <- seq.int(1L, n_parts - 1L)
    use_parallel <- isTRUE(getOption("skiLift.parallel_fetch", TRUE)) &&
                    length(remaining) > 1L

    if (use_parallel) {
      rest <- sf_fetch_partitions_parallel(con, handle, remaining, meta)
    } else {
      rest <- .fetch_partitions_sequential(con, handle, remaining, meta)
    }
    combined <- do.call(rbind, c(list(first_frame), rest))
  }

  if (is.null(combined) || nrow(combined) == 0L) {
    combined <- .empty_df_from_meta(meta)
  }

  nanoarrow::as_nanoarrow_array_stream(combined)
}


#' Fetch a single JSON partition and return as a nanoarrow_array_stream
#'
#' @param con SnowflakeConnection.
#' @param resp_body Initial response body (for partition 0).
#' @param meta Metadata from sf_parse_metadata().
#' @param partition 0-based partition index.
#' @returns A nanoarrow_array_stream.
#' @noRd
sf_fetch_partition_as_arrow_stream <- function(con, resp_body, meta, partition) {
  rlang::check_installed("nanoarrow", reason = "for Arrow result format")

  if (partition == 0L) {
    parsed <- sf_parse_response(resp_body)
    df <- parsed$data
  } else {
    part_resp <- sf_api_fetch_partition(con, meta$statement_handle, partition)
    df <- .parse_partition_data(part_resp, meta)
  }

  if (is.null(df) || nrow(df) == 0L) {
    df <- .empty_df_from_meta(meta)
  }

  nanoarrow::as_nanoarrow_array_stream(df)
}
