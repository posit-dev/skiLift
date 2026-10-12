#' @keywords internal
"_PACKAGE"

#' @import DBI
#' @import methods
#' @importFrom rlang %||% abort inform warn is_empty :=
#' @importFrom utils capture.output head str getFromNamespace
#' @importFrom cli cli_inform cli_warn cli_abort
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom httr2 request req_headers req_body_json req_perform
#'   resp_status resp_body_json req_timeout req_retry
#'   req_url_path_append resp_body_string resp_header resp_content_type
#'   req_error req_method
NULL

# SQL function names used inside dbplyr::sql_expr() in R/dbplyr.R. They are
# written as R calls but never evaluated: dbplyr captures them and emits them
# as Snowflake SQL, so R CMD check cannot see a definition for them.
utils::globalVariables(c(
  "APPROX_COUNT_DISTINCT", "APPROX_PERCENTILE", "ARRAY_AGG", "ARRAY_CONTAINS",
  "ARRAY_SIZE", "ARRAY_SLICE", "ARRAY_UNIQUE_AGG", "IS_ARRAY", "IS_INTEGER",
  "IS_OBJECT", "MODE", "OBJECT_AGG", "PARSE_JSON", "REGEXP_SUBSTR",
  "TRY_PARSE_JSON", "TYPEOF"
))
