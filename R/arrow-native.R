# Native Arrow Transport
# =============================================================================
# When a session-based connection is active and use_native_arrow is enabled,
# results are fetched as native Apache Arrow IPC binary instead of JSON.
# The internal protocol returns:
#   - First partition: base64-encoded Arrow IPC in `rowsetBase64`
#   - Subsequent partitions: pre-signed cloud URLs in `chunks[].url`
#
# This provides 5-10x improvement for large results by avoiding JSON
# string serialization/deserialization.

#' Decode base64-encoded Arrow IPC data to a nanoarrow array stream
#'
#' @param b64_string Base64-encoded Arrow IPC bytes.
#' @returns A nanoarrow_array_stream, or NULL on failure.
#' @noRd
sf_decode_arrow_base64 <- function(b64_string) {
  rlang::check_installed("nanoarrow", reason = "for native Arrow transport")

  if (is.null(b64_string) || !nzchar(b64_string)) return(NULL)

  raw_bytes <- tryCatch(
    openssl::base64_decode(b64_string),
    error = function(e) {
      cli_warn("Failed to decode base64 Arrow data: {conditionMessage(e)}")
      NULL
    }
  )
  if (is.null(raw_bytes)) return(NULL)

  tryCatch(
    nanoarrow::read_nanoarrow(raw_bytes),
    error = function(e) {
      cli_warn("Failed to parse Arrow IPC data: {conditionMessage(e)}")
      NULL
    }
  )
}


#' Download an Arrow IPC chunk from a pre-signed cloud URL
#'
#' @param url Pre-signed URL for the chunk.
#' @param chunk_headers Named list of HTTP headers (from `chunkHeaders`).
#' @param qrmk Query Result Master Key for SSE-C decryption, or NULL.
#' @returns A nanoarrow_array_stream, or NULL on failure.
#' @noRd
sf_download_arrow_chunk <- function(url, chunk_headers = NULL, qrmk = NULL) {
  rlang::check_installed("nanoarrow", reason = "for native Arrow transport")

  req <- httr2::request(url) |>
    httr2::req_timeout(getOption("skiLift.timeout", 600L)) |>
    httr2::req_error(is_error = function(resp) FALSE)

  if (!is.null(chunk_headers)) {
    for (hdr_name in names(chunk_headers)) {
      req <- req |> httr2::req_headers(!!hdr_name := chunk_headers[[hdr_name]])
    }
  }

  resp <- tryCatch(
    httr2::req_perform(req),
    error = function(e) {
      cli_warn("Failed to download Arrow chunk: {conditionMessage(e)}")
      NULL
    }
  )
  if (is.null(resp) || httr2::resp_status(resp) != 200L) return(NULL)

  raw_bytes <- httr2::resp_body_raw(resp)

  # SSE-C decryption if qrmk is provided
  if (!is.null(qrmk) && nzchar(qrmk)) {
    raw_bytes <- .decrypt_arrow_chunk(raw_bytes, qrmk)
    if (is.null(raw_bytes)) return(NULL)
  }

  tryCatch(
    nanoarrow::read_nanoarrow(raw_bytes),
    error = function(e) {
      cli_warn("Failed to parse downloaded Arrow chunk: {conditionMessage(e)}")
      NULL
    }
  )
}


#' Decrypt an SSE-C encrypted Arrow chunk
#'
#' Uses AES-256-CBC with the query result master key.
#'
#' @param raw_bytes Raw encrypted bytes.
#' @param qrmk Base64-encoded query result master key.
#' @returns Decrypted raw bytes, or NULL on failure.
#' @noRd
.decrypt_arrow_chunk <- function(raw_bytes, qrmk) {
  rlang::check_installed("openssl", reason = "for SSE-C chunk decryption")

  tryCatch({
    key <- openssl::base64_decode(qrmk)
    # IV is prepended to the ciphertext (first 16 bytes)
    iv <- raw_bytes[1:16]
    ciphertext <- raw_bytes[17:length(raw_bytes)]
    openssl::aes_cbc_decrypt(ciphertext, key = key, iv = iv)
  }, error = function(e) {
    cli_warn("SSE-C decryption failed: {conditionMessage(e)}")
    NULL
  })
}


#' Fetch all partitions as native Arrow streams
#'
#' Handles the internal protocol response format with `rowsetBase64` for the
#' first partition and `chunks[].url` for subsequent partitions.
#'
#' @param con SnowflakeConnection with active session.
#' @param resp Internal protocol response body.
#' @returns A nanoarrow_array_stream combining all partitions.
#' @noRd
sf_fetch_all_native_arrow <- function(con, resp) {
  rlang::check_installed("nanoarrow", reason = "for native Arrow transport")

  data_section <- resp$data %||% resp

  # First partition: inline base64 Arrow IPC
  first_stream <- sf_decode_arrow_base64(data_section$rowsetBase64)
  if (is.null(first_stream)) {
    cli_abort("Native Arrow: failed to decode first partition (rowsetBase64).")
  }
  first_df <- as.data.frame(first_stream)

  # Subsequent partitions from pre-signed URLs
  chunks <- data_section$chunks
  if (is.null(chunks) || length(chunks) == 0L) {
    return(nanoarrow::as_nanoarrow_array_stream(first_df))
  }

  chunk_headers <- data_section$chunkHeaders %||% list()
  qrmk <- data_section$qrmk %||% NULL

  # Download chunks (parallel if enabled)
  chunk_urls <- vapply(chunks, function(c) c$url %||% "", character(1))
  chunk_urls <- chunk_urls[nzchar(chunk_urls)]

  if (length(chunk_urls) == 0L) {
    return(nanoarrow::as_nanoarrow_array_stream(first_df))
  }

  use_parallel <- isTRUE(getOption("skiLift.parallel_fetch", TRUE)) &&
                  length(chunk_urls) > 1L

  if (use_parallel) {
    n_workers <- .resolve_n_workers(length(chunk_urls))
    chunk_frames <- tryCatch({
      cl <- parallel::makeCluster(n_workers)
      on.exit(parallel::stopCluster(cl), add = TRUE)
      parallel::clusterEvalQ(cl, {
        loadNamespace("skiLift")
        loadNamespace("httr2")
        loadNamespace("nanoarrow")
      })
      parallel::clusterExport(cl, c("chunk_headers", "qrmk"),
                              envir = environment())
      parallel::parLapply(cl, chunk_urls, function(url) {
        stream <- sf_download_arrow_chunk(url, chunk_headers, qrmk)
        if (!is.null(stream)) as.data.frame(stream) else NULL
      })
    }, error = function(e) NULL)
  } else {
    chunk_frames <- lapply(chunk_urls, function(url) {
      stream <- sf_download_arrow_chunk(url, chunk_headers, qrmk)
      if (!is.null(stream)) as.data.frame(stream) else NULL
    })
  }

  # Filter out NULL results and combine
  chunk_frames <- Filter(Negate(is.null), chunk_frames)
  all_frames <- c(list(first_df), chunk_frames)
  combined <- do.call(rbind, all_frames)

  nanoarrow::as_nanoarrow_array_stream(combined)
}


#' Check if native Arrow transport is available for a connection
#' @noRd
.can_use_native_arrow <- function(conn) {
  isTRUE(getOption("skiLift.use_native_arrow", FALSE)) &&
    .has_session(conn) &&
    requireNamespace("nanoarrow", quietly = TRUE)
}
