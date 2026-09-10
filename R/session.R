# Session Management (Internal Snowflake Protocol)
# =============================================================================
# Implements the /session/v1/* endpoints for establishing persistent sessions.
# A session provides a session token + master token, enabling:
#   - Multi-statement transactions (BEGIN/COMMIT/ROLLBACK)
#   - Temporary tables that persist across requests
#   - Session variables
#   - (Future) Native Arrow transport via /queries/v1/query-request
#
# Session-based auth is opt-in via options(skiLift.use_session = TRUE).

#' Log in to Snowflake and obtain session + master tokens
#'
#' @param account Account identifier.
#' @param auth Auth list from sf_auth_resolve() (must contain token + type info).
#' @returns A list with `token`, `masterToken`, `sessionId`, and `validityInSeconds`.
#' @noRd
sf_session_login <- function(account, auth) {
  host <- sf_host(account)
  url <- paste0(host, "/session/v1/login-request")

  login_body <- list(
    data = list(
      CLIENT_APP_ID       = "skiLift",
      CLIENT_APP_VERSION  = as.character(utils::packageVersion("skiLift")),
      ACCOUNT_NAME        = toupper(account),
      CLIENT_ENVIRONMENT  = list(
        APPLICATION = "skiLift",
        OS          = Sys.info()[["sysname"]],
        OS_VERSION  = Sys.info()[["release"]]
      )
    )
  )

  # Attach auth credentials depending on type
  if (identical(auth$type, "externalbrowser")) {
    # snowflakeauth already performs a login exchange as part of the browser
    # flow, so what it returns is a session-scoped token. Logging in again
    # here would be both redundant and unauthenticated -- the raw token is
    # deliberately not exposed, only the finished Authorization header.
    cli_abort(c(
      "Session mode is not supported with external browser authentication.",
      "i" = "Unset {.code options(skiLift.use_session = TRUE)}, or use",
      " " = "key-pair or PAT authentication for multi-statement transactions."
    ))
  }

  if (auth$type == "jwt") {
    login_body$data$TOKEN          <- auth$token
    login_body$data$AUTHENTICATOR  <- "SNOWFLAKE_JWT"
    login_body$data$LOGIN_NAME     <- toupper(auth$user %||% "")
  } else if (auth$type %in% c("pat", "token")) {
    login_body$data$TOKEN          <- auth$token
    login_body$data$AUTHENTICATOR  <- "OAUTH"
    login_body$data$LOGIN_NAME     <- toupper(auth$user %||% "")
  }

  req <- httr2::request(url) |>
    httr2::req_headers(
      "Content-Type"  = "application/json",
      "Accept"        = "application/json",
      "User-Agent"    = sf_user_agent()
    ) |>
    httr2::req_body_json(login_body, auto_unbox = TRUE) |>
    httr2::req_timeout(getOption("skiLift.timeout", 600L)) |>
    httr2::req_error(is_error = function(resp) FALSE)

  resp <- tryCatch(
    httr2::req_perform(req),
    error = function(e) {
      cli_abort(c(
        "x" = "Session login request failed.",
        "x" = conditionMessage(e)
      ))
    }
  )

  status <- httr2::resp_status(resp)
  body <- httr2::resp_body_json(resp)

  if (status != 200L || !isTRUE(body$success)) {
    msg <- body$message %||% body$data$errorMessage %||% "(unknown error)"
    cli_abort(c(
      "x" = "Session login failed (HTTP {status}).",
      "i" = msg
    ))
  }

  data <- body$data
  list(
    token             = data$token,
    master_token      = data$masterToken,
    session_id        = data$sessionId,
    validity_seconds  = as.integer(data$validityInSecondsST %||% 14400L),
    created_at        = Sys.time()
  )
}


#' Renew a session token using the master token
#'
#' @param account Account identifier.
#' @param session Session list from sf_session_login().
#' @returns Updated session list with new token.
#' @noRd
sf_session_renew <- function(account, session) {
  host <- sf_host(account)
  url <- paste0(host, "/session/token-request")

  body <- list(
    REQUEST_TYPE = "RENEW",
    oldSessionToken = session$token,
    masterToken = session$master_token
  )

  req <- httr2::request(url) |>
    httr2::req_headers(
      "Content-Type"  = "application/json",
      "Accept"        = "application/json",
      "Authorization" = paste("Snowflake Token=\"\"")
    ) |>
    httr2::req_body_json(body, auto_unbox = TRUE) |>
    httr2::req_timeout(30L) |>
    httr2::req_error(is_error = function(resp) FALSE)

  resp <- tryCatch(httr2::req_perform(req), error = function(e) NULL)
  if (is.null(resp)) return(session)

  resp_body <- tryCatch(httr2::resp_body_json(resp), error = function(e) list())
  if (isTRUE(resp_body$success) && !is.null(resp_body$data$sessionToken)) {
    session$token      <- resp_body$data$sessionToken
    session$created_at <- Sys.time()
  }
  session
}


#' Send a heartbeat to keep the session alive
#'
#' @param account Account identifier.
#' @param session Session list.
#' @returns TRUE on success, FALSE on failure.
#' @noRd
sf_session_heartbeat <- function(account, session) {
  host <- sf_host(account)
  url <- paste0(host, "/session/heartbeat")

  req <- httr2::request(url) |>
    httr2::req_headers(
      "Authorization" = paste("Snowflake Token=", paste0('"', session$token, '"')),
      "Content-Type"  = "application/json"
    ) |>
    httr2::req_method("POST") |>
    httr2::req_timeout(15L) |>
    httr2::req_error(is_error = function(resp) FALSE)

  resp <- tryCatch(httr2::req_perform(req), error = function(e) NULL)
  !is.null(resp) && httr2::resp_status(resp) == 200L
}


#' Delete (close) a session
#'
#' @param account Account identifier.
#' @param session Session list.
#' @returns TRUE on success, FALSE on failure.
#' @noRd
sf_session_delete <- function(account, session) {
  host <- sf_host(account)
  url <- paste0(host, "/session")

  req <- httr2::request(url) |>
    httr2::req_headers(
      "Authorization" = paste("Snowflake Token=", paste0('"', session$token, '"')),
      "Content-Type"  = "application/json"
    ) |>
    httr2::req_body_json(list(delete = TRUE), auto_unbox = TRUE) |>
    httr2::req_method("POST") |>
    httr2::req_timeout(15L) |>
    httr2::req_error(is_error = function(resp) FALSE)

  resp <- tryCatch(httr2::req_perform(req), error = function(e) NULL)
  !is.null(resp) && httr2::resp_status(resp) == 200L
}


#' Check if a session needs renewal (token approaching expiry)
#' @noRd
.session_needs_renewal <- function(session) {
  if (is.null(session) || is.null(session$token)) return(FALSE)
  elapsed <- as.numeric(difftime(Sys.time(), session$created_at, units = "secs"))
  validity <- session$validity_seconds %||% 14400L
  # Renew when 75% of validity has elapsed
  elapsed > (validity * 0.75)
}


#' Check if a connection has an active session
#' @noRd
.has_session <- function(conn) {
  !is.null(conn@.state$session) && !is.null(conn@.state$session$token)
}


#' Ensure the session token is fresh, renewing if needed
#' @noRd
.ensure_session_fresh <- function(conn) {
  session <- conn@.state$session
  if (is.null(session)) return(invisible())

  if (.session_needs_renewal(session)) {
    conn@.state$session <- sf_session_renew(conn@account, session)
  }
  invisible()
}
