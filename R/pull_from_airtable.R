#' Download Airtable Data
#'
#' Downloads data from Airtable and peforms some basic cleaning.
#'
#' The cleaning steps are:
#' * Make variable names lowercase and replace spaces with underscores
#' * Convert specified list-columns to regular columns (these should only be list-columns of length 1)
#' * In logical variables, replace NA with FALSE
#' * In character variables, remove leading and trailing whitespace
#'
#' @param base Airtable Base object
#' @param table Airtable Table names
#' @param unnest_cols
#'   [List-columns](https://jennybc.github.io/purrr-tutorial/ls13_list-columns.html)
#'    to convert in
#'   [tidyselect](https://tidyselect.r-lib.org/reference/language.html)
#'   format.
#' @param ... Options to pass to [airtabler::air_select()].
#'
#' @return A tibble with the cleaned data.
#' @export
#'
#' @examples
#' \dontrun{
#'   base = connect_to_airtable()
#'   pull_from_airtable(base, "Samples")
#'   pull_from_airtable(base, "Venues", unnest_cols = final_peh_estimate)
#' }
pull_from_airtable = function(base, table, unnest_cols, ...) {
  # download all the records from the table
  base[[table]][["select_all"]](...) %>%
    # clean up the variable names (lowercase and spaces to underscores)
    dplyr::rename_with(fix_names) %>%
    # convert requested list columns
    tidyr::unnest({{ unnest_cols }}) %>%
    dplyr::mutate(
      # in logical variables replace NA with FALSE
      dplyr::across(tidyselect::vars_select_helpers$where(is.logical), tidyr::replace_na, FALSE),
      # trim leading and trailing whitespace in all character variables
      dplyr::across(tidyselect::vars_select_helpers$where(is.character), trimws)
    )
}

#' Fix Variable Names
#'
#' Make variable names lower case and switch spaces to underscores.
#'
#' @param name Variable name as a string.
#'
#' @keywords internal
fix_names = function(name) {
  tolower(
    # swap spaces for underscores
    gsub(" ", "_", name)
  )
}

