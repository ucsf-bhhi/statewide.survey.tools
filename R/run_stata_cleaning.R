#' Runs Stata REDCap data cleaning code.
#'
#' Runs the included code for cleaning the raw REDCap data download. A different
#' cleaning do file can also be used.
#'
#' @param data Tibble or data frame to pass to Stata.
#' @param do_file Path to the cleaning do file. Defaults to included cleaning
#'   code.
#' @param quiet Whether to suppress printing Stata output to the screen.
#' @param ... Other options for controlling Stata interaction. Passed to
#'   [RStata::stata()]
#'
#' @return A tibble with the cleaned data.
#' @export
run_stata_cleaning = function(
  data,
  do_file = default_cleaning_do_file(),
  quiet = TRUE,
  ...
) {
  RStata::stata(
    src = do_file,
    data.in = data,
    data.out = TRUE,
    stata.echo = !quiet,
    stata.path = get_stata_path(),
    stata.version = get_stata_version(),
    ...
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
  path = getOption("RStata.StataPath", Sys.getenv("STATA_PATH"))
  if (is.null(path) )
    path = Sys.which("stata")

  if (is.null(path) | path == "")
    stop("Cannot find path to Stata executible.")

  path
}

#' Returns Stata version
#'
#' @return Stata version as integer.
get_stata_version = function() {
  version = getOption("RStata.StataVersion", Sys.getenv("STATA_VERSION"))
  if (is.null(version)) {
    version = 17L
  }

  as.integer(version)
}
