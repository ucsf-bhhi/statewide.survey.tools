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
