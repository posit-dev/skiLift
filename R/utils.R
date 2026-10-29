sf_user_agent <- function() {
  paste0(
    "skiLift/", utils::packageVersion("skiLift"),
    " R/", getRversion(),
    " (", Sys.info()[["sysname"]], ")"
  )
}

sf_host <- function(account, auth = NULL) {
  # In Workspace (SPCS), route all REST API v2 traffic to the internal
  # gateway.  Bearer + SPCS OAuth works on SNOWFLAKE_HOST for both ADBC
  # and REST API v2 (/api/v2/statements), so no public endpoint is needed.
  #
  # host_eligible, not just type == "oauth": a connections.toml OAuth
  # token (Posit Workbench / the Native App) is also type "oauth" but is
  # minted for the public endpoint, not the internal gateway -- routing it
  # to SNOWFLAKE_HOST would be a misroute if that variable is ever set
  # alongside a local profile. sf_auth_resolve() only sets host_eligible
  # for genuine Workspace/SPCS OAuth.
  spcs_host <- Sys.getenv("SNOWFLAKE_HOST", "")
  if (nzchar(spcs_host) && !is.null(auth) && auth$type == "oauth" &&
        isTRUE(auth$host_eligible)) {
    return(paste0("https://", spcs_host))
  }
  # Underscores are not valid in host names, and some TLS stacks reject them
  # when matching Snowflake's certificate. Snowflake accepts the same account
  # with each underscore written as a hyphen, for exactly this case. Only the
  # host changes: the account as given is still what the JWT claims and
  # profiles use.
  paste0("https://", .sf_hostname(account))
}

#' The public host name for an account, without the scheme
#' @noRd
.sf_hostname <- function(account) {
  paste0(gsub("_", "-", account, fixed = TRUE), ".snowflakecomputing.com")
}
