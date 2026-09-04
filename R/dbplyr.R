# dbplyr Backend for Snowflake
# =============================================================================
# Registers SnowflakeConnection as a dbplyr backend, inheriting all of
# dbplyr's built-in Snowflake SQL translations (paste0 -> CONCAT, IFF,
# ARRAY_TO_STRING, lubridate date functions, etc.) and adding skiLift-
# specific translations for Snowflake functions that dbplyr does not cover
# (semi-structured data, array operations, approximate aggregates).
#
# These S3 methods are registered lazily via .onLoad because the generics
# live in dbplyr (a Suggests dependency, not necessarily loaded).

dbplyr_edition.SnowflakeConnection <- function(con) 2L

sql_translation.SnowflakeConnection <- function(con) {
  rlang::check_installed("dbplyr", reason = "for dplyr integration")
  base <- dbplyr::sql_translation(dbplyr::simulate_snowflake())

  dbplyr::sql_variant(
    dbplyr::sql_translator(
      .parent = base$scalar,

      # -- Semi-structured data -----------------------------------------------
      parse_json     = function(x) dbplyr::sql_expr(PARSE_JSON(!!x), con = con),
      try_parse_json = function(x) dbplyr::sql_expr(TRY_PARSE_JSON(!!x), con = con),
      typeof         = function(x) dbplyr::sql_expr(TYPEOF(!!x), con = con),
      is_object      = function(x) dbplyr::sql_expr(IS_OBJECT(!!x), con = con),
      is_array       = function(x) dbplyr::sql_expr(IS_ARRAY(!!x), con = con),
      is_integer     = function(x) dbplyr::sql_expr(IS_INTEGER(!!x), con = con),

      # -- Array operations ---------------------------------------------------
      array_size     = function(x) dbplyr::sql_expr(ARRAY_SIZE(!!x), con = con),
      array_contains = function(arr, val)
        dbplyr::sql_expr(ARRAY_CONTAINS(!!val, !!arr), con = con),
      array_slice    = function(arr, from, to)
        dbplyr::sql_expr(ARRAY_SLICE(!!arr, !!from, !!to), con = con),

      # -- String -------------------------------------------------------------
      regexp_substr  = function(x, pattern, pos = 1L, occ = 1L, params = "c")
        dbplyr::sql_expr(
          REGEXP_SUBSTR(!!x, !!pattern, !!pos, !!occ, !!params), con = con
        ),

      # -- Utility ------------------------------------------------------------
      hash = function(...) {
        args <- vapply(list(...), function(a) {
          as.character(dbplyr::escape(a, con = con))
        }, character(1))
        dbplyr::sql(paste0("HASH(", paste(args, collapse = ", "), ")"))
      }
    ),

    dbplyr::sql_translator(
      .parent = base$aggregate,

      # -- Semi-structured aggregates -----------------------------------------
      object_agg       = function(key, value)
        dbplyr::sql_expr(OBJECT_AGG(!!key, !!value), con = con),
      array_agg        = function(x)
        dbplyr::sql_expr(ARRAY_AGG(!!x), con = con),
      array_unique_agg = function(x)
        dbplyr::sql_expr(ARRAY_UNIQUE_AGG(!!x), con = con),

      # -- Approximate aggregates ---------------------------------------------
      approx_count_distinct = function(x)
        dbplyr::sql_expr(APPROX_COUNT_DISTINCT(!!x), con = con),
      approx_percentile     = function(x, p)
        dbplyr::sql_expr(APPROX_PERCENTILE(!!x, !!p), con = con),

      # -- Statistical (mode_val to avoid shadowing base::mode) ---------------
      mode_val = function(x) dbplyr::sql_expr(MODE(!!x), con = con)
    ),

    dbplyr::sql_translator(
      .parent = base$window,
      approx_count_distinct = dbplyr::win_aggregate("APPROX_COUNT_DISTINCT"),
      approx_percentile     = dbplyr::win_aggregate_2("APPROX_PERCENTILE"),
      array_agg             = dbplyr::win_aggregate("ARRAY_AGG"),
      array_unique_agg      = dbplyr::win_aggregate("ARRAY_UNIQUE_AGG"),
      mode_val              = dbplyr::win_aggregate("MODE"),
      object_agg            = dbplyr::win_aggregate_2("OBJECT_AGG")
    )
  )
}

db_connection_describe.SnowflakeConnection <- function(con) {
  paste0(
    "Snowflake ", con@account,
    " [", con@database, ".", con@schema, "]"
  )
}

sql_query_save.SnowflakeConnection <- function(con, sql, name,
                                                temporary = TRUE, ...) {
  rlang::check_installed("dbplyr")
  tmp <- if (temporary) "TEMPORARY " else ""
  dbplyr::build_sql(
    "CREATE ", dbplyr::sql(tmp), "TABLE ", dbplyr::as.sql(name, con = con),
    " AS\n", sql,
    con = con
  )
}

#' Register dbplyr S3 methods (called from .onLoad)
#' @noRd
.register_dbplyr_methods <- function() {
  s3_register <- function(generic, class, method = NULL) {
    stopifnot(is.character(generic), length(generic) == 1L)
    stopifnot(is.character(class), length(class) == 1L)

    pieces <- strsplit(generic, "::")[[1L]]
    stopifnot(length(pieces) == 2L)
    package <- pieces[[1L]]
    name    <- pieces[[2L]]

    if (is.null(method)) {
      method <- get(paste0(name, ".", class), envir = parent.frame())
    }

    if (isNamespaceLoaded(package)) {
      registerS3method(name, class, method, envir = asNamespace(package))
    }

    setHook(
      packageEvent(package, "onLoad"),
      function(...) {
        registerS3method(name, class, method, envir = asNamespace(package))
      }
    )
  }

  s3_register("dbplyr::dbplyr_edition",       "SnowflakeConnection")
  s3_register("dbplyr::sql_translation",       "SnowflakeConnection")
  s3_register("dbplyr::db_connection_describe","SnowflakeConnection")
  s3_register("dbplyr::sql_query_save",        "SnowflakeConnection")
}
