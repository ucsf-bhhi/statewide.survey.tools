% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/redcap_utils.R
\name{redcap_token}
\alias{redcap_token}
\title{Fetch appropriate REDCap API Token}
\usage{
redcap_token(project = "main")
}
\arguments{
\item{project}{Either "main" (default) for the main REDCap project or "rds"
for the RDS project.}
}
\value{
The API token.
}
\description{
Fetches the REDCap API Token for the main project or the RDS project.
}
\details{
For the main project, it expects an API token in an environment variable
\code{MAIN_REDCAP_API_KEY}. To start R session with the initialized environment
variable create an .Renviron file with a line like this:
\verb{MAIN_REDCAP_API_KEY=<YOUR API TOKEN>}.

For the RDS project, it expects an API token in an environment variable
\code{RDS_REDCAP_API_KEY}. To start R session with the initialized environment
variable add a line like this to the .Renviron file:
\verb{RDS_REDCAP_API_KEY=<YOUR RDS API TOKEN>}.

To get your API token, ask one of the BHHI REDCap admins to grant you API
privileges and then request a token. See
https://redcap.ucsf.edu/api/help/?content=tokens for details.
}
