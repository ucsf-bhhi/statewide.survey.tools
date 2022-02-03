#' Downloads REDCap Data
#'
#' Downloads data from REDCap by wrapping [REDCapR::redcap_read()]. The API URL
#' is filled automatically, but can be overridden. The token for the main
#' project is also filled automatically, but can be overridden or replaced with
#' the RDS project token.
#'
#' @param ... Options passed to [REDCapR::redcap_read()].
#' @param redcap_uri REDCap API URL. Defaults to [redcap_api_url()].
#' @param token REDCap API Token. Defaults to main project token. Use RDS
#'   project token with `redcap_token("rds")`.
#' @param verbose Should messages be printed to the R console during the
#'   operation. The verbose output might contain sensitive information (e.g.
#'   PHI), so turn this off if the output might be visible somewhere public.
#'
#' @return A tibble with the requested data.
#' @export
#' @seealso [redcap_token()]
fetch_redcap_data = function(
  ...,
  redcap_uri = redcap_api_url(),
  token = redcap_token(),
  verbose = FALSE
) {
  REDCapR::redcap_read_oneshot(
    ...,
    redcap_uri = redcap_uri,
    token = token,
    verbose = verbose
  )$data %>%
    dplyr::as_tibble()
}
