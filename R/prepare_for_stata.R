prepare_for_stata = function(data) {
  dplyr::rename_with(data, stata_safe_variable_names, everything())
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

