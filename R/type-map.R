# Snowflake <-> R Type Mapping
# =============================================================================

#' Convert a character vector of JSON values to the appropriate R type
#'
#' The R type of a result column is decided by the column's Snowflake type,
#' precision and scale -- never by the values in the result. Snowflake stores
#' INTEGER, BIGINT, SMALLINT and NUMBER(38,0) identically, so for whole
#' numbers the precision is all there is to go on; see `.convert_fixed()`.
#' Converting a zero-length vector gives the right empty column, which is how
#' zero-row results get the same classes as non-empty ones.
#'
#' @param x Character vector (may contain NA).
#' @param sf_type Lowercase Snowflake type string.
#' @param scale Integer scale.
#' @param precision Integer precision (`NA` or `0` if unknown).
#' @param bigint How to return whole numbers too large for an R integer; see
#'   the `bigint` argument of `dbConnect()`.
#' @param tz_out Time zone for `TIMESTAMP_LTZ` and `TIMESTAMP_TZ` values; see
#'   the `timezone_out` argument of `dbConnect()`.
#' @returns Typed R vector.
#' @noRd
sf_convert_column <- function(x, sf_type, scale = 0L, precision = NA_integer_,
                              bigint = "numeric", tz_out = "UTC") {
  switch(sf_type,
    "fixed" = .convert_fixed(x, scale, precision, bigint),
    "real"  = , "float" = .parse_double(x),
    "boolean" = .convert_boolean(x),
    "date"    = .convert_date(x),
    "time"    = .convert_time(x),
    "timestamp_ntz" = .convert_timestamp(x, tz = "UTC"),
    "timestamp_ltz" = .convert_timestamp(x, tz = tz_out),
    "timestamp_tz"  = .convert_timestamp_tz(x, tz = tz_out),
    "binary"  = .convert_binary(x),
    "text" = x,
    x
  )
}


# ---------------------------------------------------------------------------
# Type conversion helpers
# ---------------------------------------------------------------------------

.bigint_choices <- c("numeric", "integer64", "integer", "character")

#' Whole numbers (scale 0) by precision; everything else with a scale is double
#'
#' - precision up to 9 digits always fits an R integer, so it is `integer`;
#' - anything larger follows `bigint`, default `numeric` (a double, exact to
#'   2^53).
#'
#' A plain INTEGER column is NUMBER(38,0), so it takes the `bigint` branch.
#' `integer64` is not the default because many R functions do not understand
#' it and some silently give wrong answers (`lm()`, `data.matrix()`,
#' `reticulate::r_to_py()`), and in Snowflake nearly every whole-number column
#' would be one.
#' Values that do not fit the chosen type come back `NA` with a warning,
#' never silently.
#' @noRd
.convert_fixed <- function(x, scale, precision = NA_integer_, bigint = "numeric") {
  if (is.na(scale) || scale != 0L) return(.parse_double(x))
  if (!is.na(precision) && precision > 0L && precision <= 9L) return(as.integer(x))
  switch(bigint,
    "integer64" = {
      rlang::check_installed("bit64", reason = 'to return whole numbers as integer64 (`bigint = "integer64"`).')
      .whole_number(x, bit64::as.integer64, "integer64")
    },
    "integer"   = .whole_number(x, as.integer, "integer"),
    "numeric"   = .parse_double(x),
    "character" = x,
    cli::cli_abort("Unknown {.arg bigint} value {.val {bigint}}.")
  )
}

.whole_number <- function(x, convert, what) {
  out <- suppressWarnings(convert(x))
  lost <- is.na(out) & !is.na(x)
  if (any(lost)) {
    cli::cli_warn(c(
      "{sum(lost)} whole-number value{?s} did not fit {what} and {?was/were} returned as {.code NA}.",
      "i" = 'Use {.code bigint = "numeric"} or {.code bigint = "character"} in {.fn dbConnect} to keep them.'
    ))
  }
  out
}


# A BINARY column. The SQL API sends hex text when binary_output_format is HEX,
# which sf_api_submit() asks for on every statement. The result is a blob (a
# list of raw vectors, NULL for NA), as DBI expects; a plain list of raw if the
# blob package is not installed.
.convert_binary <- function(x) {
  raws <- lapply(x, function(h) {
    if (is.na(h)) return(NULL)
    if (!nzchar(h)) return(raw(0))
    as.raw(strtoi(substring(h, seq(1L, nchar(h), 2L), seq(2L, nchar(h), 2L)), 16L))
  })
  if (requireNamespace("blob", quietly = TRUE)) blob::new_blob(raws) else raws
}

# A plain list of raw vectors (or NULL for NA): DBI's other way of holding binary.
.is_raw_list <- function(obj) {
  is.list(obj) && !is.data.frame(obj) && length(obj) > 0L &&
    all(vapply(obj, function(x) is.null(x) || is.raw(x), logical(1)))
}

.convert_boolean <- function(x) {
  # Single lookup: "true"/"1" -> TRUE, "false"/"0" -> FALSE, NA stays NA
  lookup <- c("true" = TRUE, "false" = FALSE, "1" = TRUE, "0" = FALSE)
  unname(lookup[x])
}

.convert_date <- function(x) {
  # SQL API v2 returns dates as epoch-day integers (days since 1970-01-01).
  # Text that is not a number is read as an ISO date. There is no size limit
  # on the day count: Snowflake dates run to 9999-12-31, 2.9 million days on.
  nums <- suppressWarnings(as.numeric(x))
  if (any(is.na(nums) & !is.na(x))) return(as.Date(x))
  as.Date(nums, origin = "1970-01-01")
}

# Text to double, correctly rounded. R's own as.numeric() is not: it turns
# "1.7976931348623157e+308" into Inf and "1e-300" into a value about 29 units
# in the last place away. jsonlite's parser uses the C library's strtod, which
# is exact. Anything that is not a plain JSON number (NaN, inf, ".5") goes
# through as.numeric(), which handles those.
.parse_double <- function(x) {
  out <- rep(NA_real_, length(x))
  present <- !is.na(x)
  plain <- present & grepl("^-?(0|[1-9][0-9]*)(\\.[0-9]+)?([eE][+-]?[0-9]+)?$", x)
  if (any(plain)) {
    out[plain] <- as.numeric(jsonlite::fromJSON(paste0("[", paste(x[plain], collapse = ","), "]")))
  }
  other <- present & !plain
  if (any(other)) out[other] <- suppressWarnings(as.numeric(x[other]))
  out
}

.convert_time <- function(x) {
  # SQL API v2 returns TIME as seconds since midnight with a fractional part,
  # e.g. "45296.500000000" for 12:34:56.5. (This used to divide by 1e9 on the
  # assumption that the API sent nanoseconds, so every value came back a
  # billion times too small.)
  secs <- suppressWarnings(as.numeric(x))
  if (requireNamespace("hms", quietly = TRUE)) {
    hms::as_hms(secs)
  } else {
    secs
  }
}

.convert_timestamp <- function(x, tz = "UTC") {
  # SQL API v2 returns timestamps as epoch strings with fractional seconds,
  # OR as nanoseconds-since-epoch. The format depends on TIMESTAMP_OUTPUT_FORMAT.
  nums <- suppressWarnings(as.numeric(x))
  if (all(is.na(nums))) {
    return(as.POSIXct(rep(NA_real_, length(x)), origin = "1970-01-01", tz = tz))
  }

  # Heuristic: if values > 1e15, they are nanoseconds; if > 1e12, milliseconds
  max_val <- max(abs(nums), na.rm = TRUE)
  epoch_secs <- if (max_val > 1e15) {
    nums / 1e9
  } else if (max_val > 1e12) {
    nums / 1e3
  } else {
    nums
  }

  as.POSIXct(epoch_secs, origin = "1970-01-01", tz = tz)
}

.convert_timestamp_tz <- function(x, tz = "UTC") {
  # SQL API v2 returns TIMESTAMP_TZ as "<epoch seconds> <offset>", e.g.
  # "1791961496.000000000 1770", where the offset is minutes east of UTC plus
  # 1440 (1770 = +05:30). (This used to pass the whole string to as.numeric(),
  # which gives NA, so every value came back NA.)
  #
  # R has no vector type that holds a different UTC offset per element, so the
  # result is the correct instant shown in `tz` (the `timezone_out` setting).
  # The offset each value was stored with is not kept.
  epoch <- sub(" .*$", "", x)
  .convert_timestamp(epoch, tz = tz)
}


# ---------------------------------------------------------------------------
# R -> Snowflake type mapping (for dbDataType / dbWriteTable)
# ---------------------------------------------------------------------------

#' Map an R object to a Snowflake SQL type string
#' @noRd
r_to_sf_type <- function(obj) {
  if (inherits(obj, "Date"))          return("DATE")
  if (inherits(obj, "POSIXct"))       return("TIMESTAMP_NTZ")
  if (inherits(obj, "POSIXlt"))       return("TIMESTAMP_NTZ")
  if (inherits(obj, "difftime"))      return("TIME")
  if (inherits(obj, "hms"))           return("TIME")
  if (inherits(obj, "blob") || .is_raw_list(obj)) return("BINARY")
  if (inherits(obj, "integer64"))     return("NUMBER(19, 0)")
  if (is.integer(obj))                return("NUMBER(10, 0)")
  if (is.double(obj))                 return("DOUBLE")
  if (is.logical(obj))                return("BOOLEAN")
  if (is.character(obj))              return("TEXT")
  if (is.raw(obj))                    return("BINARY")
  "TEXT"
}
