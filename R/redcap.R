#' REDCap API URL
#'
#' Returns the URL for the REDCap API: https://redcap.ucsf.edu/api/
#'
#' @return REDCap API URL
#' @export
redcap_api_url = function() {
  "https://redcap.ucsf.edu/api/"
}

#' Fetch appropriate REDCap API Token
#'
#' Fetches the REDCap API Token for the main project or the RDS project.
#'
#' For the main project, it expects an API token in an environment variable
#' `MAIN_REDCAP_API_KEY`. To start R session with the initialized environment
#' variable create an .Renviron file with a line like this:
#' `MAIN_REDCAP_API_KEY=<YOUR API TOKEN>`.
#'
#' For the RDS project, it expects an API token in an environment variable
#' `RDS_REDCAP_API_KEY`. To start R session with the initialized environment
#' variable add a line like this to the .Renviron file:
#' `RDS_REDCAP_API_KEY=<YOUR RDS API TOKEN>`.
#'
#' To get your API token, ask one of the BHHI REDCap admins to grant you API
#' privileges and then request a token. See
#' https://redcap.ucsf.edu/api/help/?content=tokens for details.
#'
#' @param project Either "main" (default) for the main REDCap project or "rds"
#'   for the RDS project.
#'
#' @return The API token.
#' @export
redcap_token = function(project = "main") {
  if (project == "main") {
    Sys.getenv("MAIN_REDCAP_API_TOKEN")
  } else if (project == "rds") {
    Sys.getenv("RDS_REDCAP_API_TOKEN")
  } else {
    NULL
  }
}

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
  REDCapR::redcap_read(
    ...,
    redcap_uri = redcap_uri,
    token = token,
    verbose = verbose
  )$data %>%
    dplyr::as_tibble()
}
