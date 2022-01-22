#' Runs Stata REDCap data cleaning code.
#'
#' Runs the included code for cleaning the raw REDCap data download. A different
#' cleaning do file can also be used.
#'
#' @param data Tibble or data frame to pass to Stata.
#' @param do_file Path to the cleaning do file. Defaults to included cleaning
#'   code.
#' @param quiet Whether to suppress printing Stata output to the screen.
#'
#' @return A tibble with the cleaned data.
#' @export
run_stata_cleaning = function(
  data,
  do_file = default_cleaning_do_file(),
  quiet = TRUE
) {
  RStata::stata(
    src = do_file,
    data.in = data,
    data.out = TRUE,
    stata.echo = !quiet,
    stata.path = get_stata_path(),
    stata.version = get_stata_version()
  ) %>%
    dplyr::as_tibble()
}

#' Provides path to included Stata data cleaning code.
#'
#' @return Path to cleaning do file.
#' @keywords internal
default_cleaning_do_file = function() {
  system.file("stata", "data_cleaning.do", package = "statewide.survey.tools")
}

#' Returns path to Stata binary
#'
#' @return Path to Stata binary.
#' @keywords internal
get_stata_path = function() {
  path = getOption("RStata.StataPath")
  if (is.null(path)) {
    path = "C:/Program Files/Stata17/StataMP-64.exe"
  }

  path
}

#' Returns Stata version
#'
#' @return Stata version as integer.
get_stata_version = function() {
  version = getOption("RStata.StataVersion")
  if (is.null(version)) {
    version = 17L
  }

  version
}

#' Convert a Stata date to text
#'
#' Converts a Stata date to a character string.
#'
#' @param date Stata date.
#' @param format Format string for character representation. See
#'   [base::strptime()] for options. Defaults to formatting like January 1,
#'   2022.
#'
#' @return A character string with the formatted date.
#' @export
format_stata_date = function(date, format = "%B %e, %Y") {
  lubridate::dmy(date) %>%
    as.character(format)
}
