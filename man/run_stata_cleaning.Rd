% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_stata_cleaning.R
\name{run_stata_cleaning}
\alias{run_stata_cleaning}
\title{Runs Stata REDCap data cleaning code.}
\usage{
run_stata_cleaning(
  data,
  do_file = default_cleaning_do_file(),
  quiet = TRUE,
  ...
)
}
\arguments{
\item{data}{Tibble or data frame to pass to Stata.}

\item{do_file}{Path to the cleaning do file. Defaults to included cleaning
code.}

\item{quiet}{Whether to suppress printing Stata output to the screen.}

\item{...}{Other options for controlling Stata interaction. Passed to
\code{\link[RStata:stata]{RStata::stata()}}}
}
\value{
A tibble with the cleaned data.
}
\description{
Runs the included code for cleaning the raw REDCap data download. A different
cleaning do file can also be used.
}
