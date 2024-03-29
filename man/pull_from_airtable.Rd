% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pull_from_airtable.R
\name{pull_from_airtable}
\alias{pull_from_airtable}
\title{Download Airtable Data}
\usage{
pull_from_airtable(base, table, unnest_cols, ...)
}
\arguments{
\item{base}{Airtable Base object}

\item{table}{Airtable Table names}

\item{unnest_cols}{\href{https://jennybc.github.io/purrr-tutorial/ls13_list-columns.html}{List-columns}
to convert in
\href{https://tidyselect.r-lib.org/reference/language.html}{tidyselect}
format.}

\item{...}{Options to pass to \code{\link[airtabler:air_select]{airtabler::air_select()}}.}
}
\value{
A tibble with the cleaned data.
}
\description{
Downloads data from Airtable and peforms some basic cleaning.
}
\details{
The cleaning steps are:
\itemize{
\item Make variable names lowercase and replace spaces with underscores
\item Convert specified list-columns to regular columns (these should only be list-columns of length 1)
\item In logical variables, replace NA with FALSE
\item In character variables, remove leading and trailing whitespace
}
}
\examples{
\dontrun{
  base = connect_to_airtable()
  pull_from_airtable(base, "Samples")
  pull_from_airtable(base, "Venues", unnest_cols = final_peh_estimate)
}
}
