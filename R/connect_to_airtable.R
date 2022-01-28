#' Connect to the Airtable API
#'
#' Makes a connection to the Airtable API using the [airtabler::airtabler()]
#' package.
#'
#' Expects an API key in an environment variable `AIRTABLE_API_KEY`. To start R
#' session with the initialized environment variable create an .Renviron file
#' with a line like this: `AIRTABLE_API_KEY=<YOUR API KEY>`.
#'
#' See https://airtable.com/appOLCptG2wxvoGtH/api/docs#curl/authentication for
#' details on obtaining an Airtable API key.
#'
#' @param base ID for the airtable base.
#' @param tables Names of the tables to include.
#'
#' @return An Airtable base object from [airtabler::airtable()].
#' @export
connect_to_airtable = function(
  base = "appOLCptG2wxvoGtH",
  tables = c("Venues", "Samples", "Visit Check-Out")
) {
  airtabler::airtable(base = base, tables = tables)
}
