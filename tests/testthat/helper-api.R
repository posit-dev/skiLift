# Helpers for tests of the SQL API layer (retries, polling, errors, writes).
# Nothing here touches the network: responses come from httr2's mock hook, and
# sleeping is replaced so backoff and polling run instantly.

# A connection with a bare OAuth token, enough to build request headers.
.api_con <- function(database = "", type = "token") {
  new("SnowflakeConnection",
    account = "acct", user = "u", database = database, schema = "", warehouse = "",
    role = "", .auth = list(type = type, token = "t", token_type = "OAUTH"),
    .state = .new_conn_state()
  )
}

# An httr2 response with a JSON body.
.json_resp <- function(status, body = list(), headers = list()) {
  httr2::response(
    status_code = status,
    headers = c(list(`Content-Type` = "application/json"), headers),
    body = charToRaw(jsonlite::toJSON(body, auto_unbox = TRUE, null = "null"))
  )
}

# A mock that plays `steps` in order (the last one repeats). A step is a
# response, or a function(req) that returns one or throws. `$requests()` gives
# every request seen, in order, as list(method, url).
.mock_sequence <- function(steps) {
  seen <- list()
  mock <- function(req) {
    seen[[length(seen) + 1L]] <<- list(method = req$method %||% if (is.null(req$body)) "GET" else "POST",
                                       url = req$url,
                                       timeout_ms = req$options$timeout_ms)
    step <- steps[[min(length(seen), length(steps))]]
    if (is.function(step)) step(req) else step
  }
  list(mock = mock, requests = function() seen,
       urls = function() vapply(seen, function(r) r$url, ""),
       timeouts = function() vapply(seen, function(r) r$timeout_ms %||% NA_real_, 0))
}

# A "connection reset" style failure: the request never gets a response.
.network_failure <- function(req) stop("Failed to perform HTTP request.", call. = FALSE)

# Replace the sleep so waits are recorded, not slept. Returns the recorded waits.
.record_sleeps <- function(env = parent.frame()) {
  waits <- new.env()
  waits$x <- numeric()
  testthat::local_mocked_bindings(
    .sf_sleep = function(seconds) waits$x <- c(waits$x, seconds),
    .package = "skiLift", .env = env
  )
  function() waits$x
}

.request_id_of <- function(url) {
  m <- regmatches(url, regexpr("requestId=[0-9a-f-]+", url))
  if (length(m)) sub("requestId=", "", m) else NA_character_
}
