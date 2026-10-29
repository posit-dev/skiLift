# Errors users can catch by class
# =============================================================================
# Every error that comes from a Snowflake response (or from failing to get
# one) carries a class and the facts needed to act on it, so code can tell an
# authentication failure from a SQL error from a timeout without parsing text.
# The classes are documented in ?skilift-errors.
#
#   skilift_error
#   |-- skilift_error_config      local, before any request
#   |-- skilift_error_http        a response with an error status
#   |   |-- skilift_error_auth       401 (after the credentials were refreshed)
#   |   |-- skilift_error_forbidden  403
#   |   `-- skilift_error_sql        422: the SQL did not compile or run
#   |-- skilift_error_timeout     the statement or the request ran out of time
#   |-- skilift_error_retry       transient failures outlasted the retries
#   `-- skilift_error_write       a batch write stopped part-way

#' Raise a classed skiLift error
#'
#' `...` are stored on the condition as fields (`cnd$status` and so on).
#' @param message A cli message vector.
#' @param class Subclasses, most specific first; `skilift_error` is added.
#' @param parent The condition that caused this one, if any.
#' @noRd
.sf_abort <- function(message, class = character(), ..., parent = NULL,
                      call = rlang::caller_env(), .envir = parent.frame()) {
  cli_abort(
    message,
    class = c(class, "skilift_error"),
    ...,
    parent = parent,
    call = call,
    .envir = .envir
  )
}

#' The error class for an HTTP status
#' @noRd
.sf_http_class <- function(status) {
  c(
    switch(as.character(status),
      "401" = "skilift_error_auth",
      "403" = "skilift_error_forbidden",
      "408" = "skilift_error_timeout",
      "422" = "skilift_error_sql",
      if (status %in% c(429L, 500L, 502L, 503L, 504L)) "skilift_error_retry"
    ),
    "skilift_error_http"
  )
}

#' One actionable next step for a failure, as cli "i" bullets
#'
#' Messages from Snowflake say what failed; these say what to try. Kept short
#' on purpose: the troubleshooting appendix of the guide has the long version.
#' @param status HTTP status, or NA for a failure with no response.
#' @param auth_type `con@.auth$type` ("jwt", "pat", "oauth", ...), if known.
#' @param kind "timeout" for a request or statement that ran out of time.
#' @returns A character vector of hints (possibly empty).
#' @noRd
.sf_hints <- function(status = NA_integer_, auth_type = NULL, kind = NULL,
                      message = "") {
  if (identical(kind, "timeout") || identical(as.integer(status), 408L)) {
    return(c(
      "i" = "The statement ran longer than {.code options(skiLift.timeout)} allows.",
      "i" = "Look it up in Snowflake's Query History with the statement handle above."
    ))
  }
  if (is.na(status)) {
    return(c("i" = "No response arrived. Check the network, any proxy or VPN, and that the account name is right."))
  }
  switch(as.character(status),
    "401" = if (identical(auth_type, "jwt")) {
      c(
        "i" = "The public key may not be registered, or may not match the private key in use: compare {.code RSA_PUBLIC_KEY_FP} from {.code DESCRIBE USER <user>} with the key file.",
        "i" = "JWTs are time-limited: check this machine's clock.",
        "i" = "Check the account identifier and user name."
      )
    } else {
      c("i" = "The token may have expired or been revoked. Sign in again or supply a new token.")
    },
    "403" = c(
      "i" = "The SQL API may not be enabled for this account, or a network policy may be blocking this IP address (the message often names it)."
    ),
    "422" = if (grepl("does not exist or not authorized", message, fixed = TRUE)) {
      c("i" = "This is usually a missing privilege, or the wrong role, database or schema, not a sign-in problem.")
    },
    "429" = c(
      "i" = "Snowflake is limiting the request rate. Reduce parallel fetching ({.code options(skiLift.fetch_workers)}) or try again shortly."
    ),
    NULL
  )
}
