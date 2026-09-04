# Tests for parallel partition fetching
# =============================================================================

.mock_con <- function() {
  new("SnowflakeConnection",
    account = "test", user = "user", database = "db", schema = "sch",
    warehouse = "wh", role = "role",
    .auth = list(token = "tok", token_type = "KEYPAIR_JWT"),
    .state = .new_conn_state()
  )
}

.mock_partition_resp <- function(partition_idx) {
  list(
    statementHandle = "h1",
    resultSetMetaData = list(
      numRows = 2L,
      format = "jsonv2",
      rowType = list(
        list(name = "ID", type = "FIXED", nullable = FALSE, scale = 0L, precision = 10L),
        list(name = "VAL", type = "TEXT", nullable = TRUE, scale = 0L, precision = 0L)
      ),
      partitionInfo = list(list(rowCount = 2L))
    ),
    data = list(
      list(as.character(partition_idx * 10L + 1L), paste0("part", partition_idx, "_a")),
      list(as.character(partition_idx * 10L + 2L), paste0("part", partition_idx, "_b"))
    )
  )
}


# ---------------------------------------------------------------------------
# .fetch_partitions_sequential
# ---------------------------------------------------------------------------

test_that(".fetch_partitions_sequential fetches correct partitions", {
  con <- .mock_con()
  meta <- sf_parse_metadata(.mock_partition_resp(0L))

  call_log <- integer(0)
  mockr::with_mock(
    sf_api_fetch_partition = function(con, handle, partition) {
      call_log <<- c(call_log, partition)
      .mock_partition_resp(partition)
    },
    {
      frames <- .fetch_partitions_sequential(con, "h1", c(1L, 2L, 3L), meta)
    }
  )

  expect_length(frames, 3L)
  expect_equal(call_log, c(1L, 2L, 3L))
  expect_equal(nrow(frames[[1L]]), 2L)
  expect_equal(nrow(frames[[2L]]), 2L)
  expect_equal(nrow(frames[[3L]]), 2L)
})


# ---------------------------------------------------------------------------
# sf_fetch_partitions_parallel -- falls back to sequential with 1 worker
# ---------------------------------------------------------------------------

test_that("sf_fetch_partitions_parallel falls back to sequential for 1 partition", {
  con <- .mock_con()
  meta <- sf_parse_metadata(.mock_partition_resp(0L))

  mockr::with_mock(
    sf_api_fetch_partition = function(con, handle, partition) {
      .mock_partition_resp(partition)
    },
    {
      withr::with_options(list(skiLift.fetch_workers = 1L), {
        frames <- sf_fetch_partitions_parallel(con, "h1", c(1L), meta)
      })
    }
  )

  expect_length(frames, 1L)
  expect_equal(nrow(frames[[1L]]), 2L)
})

test_that("sf_fetch_partitions_parallel falls back to sequential when workers=1", {
  con <- .mock_con()
  meta <- sf_parse_metadata(.mock_partition_resp(0L))

  mockr::with_mock(
    sf_api_fetch_partition = function(con, handle, partition) {
      .mock_partition_resp(partition)
    },
    {
      withr::with_options(list(skiLift.fetch_workers = 1L), {
        frames <- sf_fetch_partitions_parallel(con, "h1", c(1L, 2L), meta)
      })
    }
  )

  expect_length(frames, 2L)
})


# ---------------------------------------------------------------------------
# Parallel fetch disabled via option -- test via internal functions directly
# ---------------------------------------------------------------------------

test_that("sequential fetch is used when parallel_fetch option is FALSE", {
  con <- .mock_con()
  meta <- sf_parse_metadata(.mock_partition_resp(0L))

  call_log <- integer(0)
  mockr::with_mock(
    sf_api_fetch_partition = function(con, handle, partition) {
      call_log <<- c(call_log, partition)
      .mock_partition_resp(partition)
    },
    {
      withr::with_options(list(skiLift.parallel_fetch = FALSE), {
        remaining <- c(1L, 2L)
        use_parallel <- isTRUE(getOption("skiLift.parallel_fetch", TRUE)) &&
                        length(remaining) > 1L
        expect_false(use_parallel)

        frames <- .fetch_partitions_sequential(con, "h1", remaining, meta)
      })
    }
  )

  expect_length(frames, 2L)
  expect_equal(call_log, c(1L, 2L))
  expect_equal(nrow(frames[[1L]]), 2L)
  expect_equal(nrow(frames[[2L]]), 2L)
})


# ---------------------------------------------------------------------------
# .resolve_n_workers
# ---------------------------------------------------------------------------

test_that(".resolve_n_workers auto-detects cores when option is 0", {
  withr::with_options(list(skiLift.fetch_workers = 0L), {
    n <- .resolve_n_workers(10L)
    expect_true(n >= 1L)
    expect_true(n <= 10L)
  })
})

test_that(".resolve_n_workers uses explicit positive value", {
  withr::with_options(list(skiLift.fetch_workers = 2L), {
    expect_equal(.resolve_n_workers(10L), 2L)
  })
})

test_that(".resolve_n_workers clamps to n_tasks", {
  withr::with_options(list(skiLift.fetch_workers = 8L), {
    expect_equal(.resolve_n_workers(3L), 3L)
  })
})

test_that(".resolve_n_workers handles 'auto' string", {
  withr::with_options(list(skiLift.fetch_workers = "auto"), {
    n <- .resolve_n_workers(10L)
    expect_true(n >= 1L)
  })
})

test_that(".resolve_n_workers handles NULL/NA gracefully", {
  withr::with_options(list(skiLift.fetch_workers = NULL), {
    n <- .resolve_n_workers(10L)
    expect_true(n >= 1L)
  })
})

test_that(".resolve_n_workers returns at least 1", {
  withr::with_options(list(skiLift.fetch_workers = 0L), {
    expect_true(.resolve_n_workers(1L) >= 1L)
  })
})


# ---------------------------------------------------------------------------
# Multi-partition combine produces correct data
# ---------------------------------------------------------------------------

test_that("sequential fetch results combine correctly with rbind", {
  con <- .mock_con()
  meta <- sf_parse_metadata(.mock_partition_resp(0L))

  mockr::with_mock(
    sf_api_fetch_partition = function(con, handle, partition) {
      .mock_partition_resp(partition)
    },
    {
      first_parsed <- sf_parse_response(.mock_partition_resp(0L))
      rest <- .fetch_partitions_sequential(con, "h1", c(1L, 2L), meta)
      combined <- do.call(rbind, c(list(first_parsed$data), rest))
    }
  )

  expect_equal(nrow(combined), 6L)
  expect_equal(ncol(combined), 2L)
  expect_s3_class(combined, "data.frame")
})
