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
  export_survey_fields = TRUE,
  for_stata = FALSE,
  col_types = NULL,
  redcap_uri = redcap_api_url(),
  token = redcap_token(),
  verbose = FALSE
) {
  if (for_stata & is.null(col_types)) {
    col_types = readr::cols(
      "age_self_report" = readr::col_double(),
      "date" = readr::col_character(),
      "episode_date" = readr::col_character(),
      "stable_time_years" = readr::col_double(),
      "yrs_ago" = readr::col_character()
    )
    if (export_survey_fields)
      col_types$cols = append(col_types$cols, list(
        "carceral_system_timestamp" = readr::col_character(),
        "children_timestamp" = readr::col_character(),
        "consent_timestamp" = readr::col_character(),
        "demographics_timestamp" = readr::col_character(),
        "discrimination_timestamp" = readr::col_character(),
        "eligibility_timestamp" = readr::col_character(),
        "end_survey_and_rds_timestamp" = readr::col_character(),
        "healthcare_utilization_timestamp" = readr::col_character(),
        "history_of_homelessness_timestamp" = readr::col_character(),
        "housing_services_timestamp" = readr::col_character(),
        "housing_trajectory_timestamp" = readr::col_character(),
        "income_employment_and_benefits_timestamp" = readr::col_character(),
        "ipv_2_timestamp" = readr::col_character(),
        "living_situation_timestamp" = readr::col_character(),
        "lumpsumsubsidy_prevention_timestamp" = readr::col_character(),
        "mental_health_3_timestamp" = readr::col_character(),
        "physical_health_timestamp" = readr::col_character(),
        "precipitants_to_homelessness_timestamp" = readr::col_character(),
        "pregnancy_timestamp" = readr::col_character(),
        "prescreen_timestamp" = readr::col_character(),
        "rehousing_timestamp" = readr::col_character(),
        "stable_housing_supplement_timestamp" = readr::col_character(),
        "substance_use_2_timestamp" = readr::col_character()
      ))
  }

  data = REDCapR::redcap_read_oneshot(
    ...,
    export_survey_fields = export_survey_fields,
    col_types = col_types,
    redcap_uri = redcap_uri,
    token = token,
    verbose = verbose
  )$data %>%
    dplyr::as_tibble()

  if (for_stata) data = prepare_for_stata(data)

  data
}
