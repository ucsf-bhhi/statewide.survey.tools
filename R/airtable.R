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

#' Prepare sampling data for Airtable
#'
#' Formats the sampling data for submission to Airtable. It selects only the
#' variables that will be sent to Airtable (internal Airtable venue id, sampling
#' weight, & selected indicator) and adds the sample metadata (ie. county, zone,
#' and round).
#'
#' @param sample Data frame with the full sample (ie. both selected and not
#'   selected sites).
#' @param venue_id Name of the column with the internal Airtable venue ID (different than
#'   the venue_id we commonly use like placer_123).
#' @param sampling_weight Name of the column with the sampling weights.
#' @param selected_indicator Name of the column which indicates whether the
#'   venue was selected.
#' @param county Character string with the county name of the sample.
#' @param zone Character string with the zone of the sample.
#' @param round Character string or number with the round of the sample.
#'
#' @return A data frame ready to be passed to [insert_airtable_records()].
#' @export
#'
#' @examples
#' \dontrun{
#'   prepare_sample_for_airtable(
#'     final_sample,
#'     id,
#'     sampling_weight,
#'     sampled,
#'     "Sonoma",
#'     "Santa Rosa",
#'     2
#'   )
#' }
prepare_sample_for_airtable = function(
  sample,
  venue_id,
  sampling_weight,
  sampled_indicator,
  county,
  zone,
  round
) {
  sample %>%
    select(
      Venue = {{ venue_id }},
      {{ sampling_weight }},
      sampled = {{ selected_indicator }}
    ) %>%
    mutate(
      Venue = as.list(Venue),
      sampling_weight = as.numeric(sampling_weight),
      sampled = as.integer(sampled),
      sample_county = county,
      sample_zone = zone,
      sample_round = as.character(round)
    )
}

#' Upload Data to Airtable
#'
#' Uploads a properly named and formatted data frame to Airtable. It handles
#' converting the data frame to JSON that Airtable understands, and splitting
#' the data into chunks of no more than 10 rows to comply with Airtable limits.
#'
#' @param data Data frame of properly named and formatted data.
#' @param base Airtable Base ID.
#' @param table Name of the Airtable table.
#' @param chunk_size Number of rows in each data chunk. Airtable accepts a
#'   maximum of 10 rows in one request, so this cannot be greater than 10.
#'   Default is 10.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   insert_airtable_records(
#'     data_for_airtable,
#'     "appOLCptG2wxvoGtH",
#'     "Samples"
#'   )
#' }
insert_airtable_records = function(
  data,
  base,
  table,
  chunk_size = 10
) {
  # break data into chunks of 10 rows each b/c the airtable api will only
  # take that many in one request
  chunk_data(data, chunk_size = 10) %>%
    purrr::walk(post_to_airtable, base = base, table = table)
}

#' Split Data Frame into Chunks
#'
#' Breaks a data frame into equal sized chunks for upload to Airtable.
#'
#' @param data A data frame or tibble.
#' @param chunk_size The number of rows in each chunk.
#'
#' @return A list of data frames or tibbles.
#'
#' @keywords internal
chunk_data = function(data, chunk_size) {
  dplyr::group_split(data, group_id = (dplyr::row_number() - 1) %/% chunk_size, .keep = FALSE)
}

#' Send Upload Request to Airtable
#'
#' Sends an HTTP POST request to Airtable. It handles converting the data frame
#' to JSON, and creating and submitting the request.
#'
#' @param data A data frame or tibble with no more than 10 rows.
#' @param base Airtable Base ID.
#' @param table Airtable table name.
#'
#' @keywords internal
post_to_airtable = function(data, base, table) {
  httr::POST(
    url = glue::glue("https://api.airtable.com/v0/{base}/{URLencode(table)}"),
    httr::add_headers(
      Authorization = paste("Bearer", Sys.getenv("AIRTABLE_API_KEY")),
      `Content-Type` = "application/json"
    ),
    body = json_for_airtable(data),
    encode = "raw"
  )
}

#' Convert Data Frame to Airtable JSON
#'
#' Converts a data frame or tibble to the JSON data format accepted by the
#' Airtable API.
#'
#' @param data Data frame/tibble with properly named and formatted columns.
#'
#'
#' @keywords internal
json_for_airtable = function(data) {
  # turn the data frame into a list of its rows
  rows = split(data, seq(nrow(data))) %>%
    stats::setNames(rep(NULL, nrow(data))) %>%
    purrr::map(~ list(fields = jsonlite::unbox(.x)))

  list(records = rows, typecast = jsonlite::unbox(TRUE)) %>%
    jsonlite::toJSON(pretty = T)
}
