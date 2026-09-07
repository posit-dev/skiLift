# JWT Generation for Key-Pair Authentication
# =============================================================================

#' Generate a Snowflake JWT from a private key file
#'
#' Creates a short-lived JWT token suitable for authenticating to
#' Snowflake's SQL API using key-pair authentication.
#'
#' @param account Snowflake account identifier.
#' @param user Snowflake username.
#' @param private_key_path Path to PEM-encoded PKCS8 private key (.p8 file).
#' @param lifetime_sec Token lifetime in seconds. Default 3540 (59 min).
#' @returns JWT token string.
#' @noRd
sf_generate_jwt <- function(account, user, private_key_path,
                            lifetime_sec = 3540L) {
  if (!requireNamespace("openssl", quietly = TRUE)) {
    cli_abort(c(
      "Package {.pkg openssl} is required for key-pair JWT authentication.",
      "i" = "Install with: {.code install.packages(\"openssl\")}"
    ))
  }

  key_path <- path.expand(private_key_path)
  if (!file.exists(key_path)) {
    cli_abort("Private key file not found: {.file {key_path}}")
  }

  # Load RSA private key
  key <- tryCatch(
    openssl::read_key(key_path),
    error = function(e) {
      cli_abort(c(
        "Failed to read private key: {.file {key_path}}",
        "x" = conditionMessage(e)
      ))
    }
  )

  # Compute SHA-256 fingerprint of the public key (DER-encoded)
  pub_key <- key$pubkey
  pub_der <- openssl::write_der(pub_key)
  fp_raw <- openssl::sha256(pub_der)
  fp_b64 <- openssl::base64_encode(fp_raw)

  acct_upper <- .jwt_account_identifier(account)
  user_upper <- toupper(user)

  now <- as.integer(Sys.time())
  qualified_user <- paste0(acct_upper, ".", user_upper)

  # JWT payload
  payload <- list(
    iss = paste0(qualified_user, ".SHA256:", fp_b64),
    sub = qualified_user,
    iat = now,
    exp = now + as.integer(lifetime_sec)
  )

  # Encode header and payload
  header <- list(alg = "RS256", typ = "JWT")
  header_b64 <- .base64url_encode(charToRaw(toJSON(header, auto_unbox = TRUE)))
  payload_b64 <- .base64url_encode(charToRaw(toJSON(payload, auto_unbox = TRUE)))

  signing_input <- paste0(header_b64, ".", payload_b64)

  # Sign with RS256
  sig_raw <- openssl::signature_create(
    charToRaw(signing_input),
    hash = openssl::sha256,
    key = key
  )
  sig_b64 <- .base64url_encode(sig_raw)

  paste0(signing_input, ".", sig_b64)
}

#' Reduce a Snowflake account identifier to the form the JWT iss/sub claims
#' expect
#'
#' Matches Snowflake's documented key-pair JWT algorithm. Most account
#' identifiers carry a region/cloud-platform suffix (e.g.
#' `ij38992.eu-west-2.aws`) that must be dropped entirely, not folded in --
#' only the account-name segment before the first dot goes in the claim.
#' The `.global` form (replicated org accounts) is the one exception: there
#' the segment before the first hyphen is the account name and the dot is
#' part of the identifier itself.
#' @noRd
.jwt_account_identifier <- function(account) {
  if (grepl("\\.global\\b", account, ignore.case = TRUE)) {
    account <- sub("-.*$", "", account)
  } else {
    account <- sub("\\..*$", "", account)
  }
  toupper(account)
}

#' Base64url encoding (no padding, URL-safe alphabet)
#' @noRd
.base64url_encode <- function(raw_bytes) {
  b64 <- openssl::base64_encode(raw_bytes)
  b64 <- gsub("=+$", "", b64)
  b64 <- gsub("+", "-", b64, fixed = TRUE)
  b64 <- gsub("/", "_", b64, fixed = TRUE)
  b64
}
