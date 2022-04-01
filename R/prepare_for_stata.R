prepare_for_stata = function(data) {
  data %>%
    dplyr::rename(
      precipitants_to_home_v_0 = precipitants_to_homelessness_timestamp,
      precipitants_to_home_v_1 = precipitants_to_homelessness_complete,
      lumpsumsubsidy_preve_v_2 = lumpsumsubsidy_prevention_timestamp,
      lumpsumsubsidy_preve_v_3 = lumpsumsubsidy_prevention_complete,
      stable_housing_suppl_v_4 = stable_housing_supplement_timestamp,
      stable_housing_suppl_v_5 = stable_housing_supplement_complete,
      history_of_homelessn_v_6 = history_of_homelessness_timestamp,
      history_of_homelessn_v_7 = history_of_homelessness_complete,
      income_employment_an_v_8 = income_employment_and_benefits_timestamp,
      income_employment_an_v_9 = income_employment_and_benefits_complete,
      healthcare_utilizati_v_10 = healthcare_utilization_timestamp
    ) %>%
    dplyr::rename_with(stata_safe_variable_names, everything())
}

stata_safe_variable_names = function(name) {
  name %>%
    # stata variables can't contain non-letters, numbers, or '_', so replace them with a '_'
    strip_stata_illegal_char() %>%
    strip_leading_digit() %>%
    stata_name_truncate()
}

strip_stata_illegal_char = function(name) {
  gsub("[^a-zA-Z0-9_]", "_", name)
}

strip_leading_digit = function(name) {
  dplyr::if_else(grepl("^[0-9]", name), paste0("_", name), name)
}

stata_name_truncate = function(name) {
  substr(name, 1, 32)
}

