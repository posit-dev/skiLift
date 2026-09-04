#' @keywords internal
"_PACKAGE"

#' @import DBI
#' @import methods
#' @importFrom rlang %||% abort inform warn is_empty
#' @importFrom cli cli_inform cli_warn cli_abort
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom httr2 request req_headers req_body_json req_perform
#'   resp_status resp_body_json req_timeout req_retry
#'   req_url_path_append resp_body_string resp_header resp_content_type
#'   req_error req_method
NULL
