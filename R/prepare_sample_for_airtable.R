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
#' @param sampled_indicator Name of the column which indicates whether the
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
    dplyr::select(
      Venue = {{ venue_id }},
      {{ sampling_weight }},
      sampled = {{ sampled_indicator }}
    ) %>%
    dplyr::mutate(
      Venue = as.list(Venue),
      sampling_weight = as.numeric(sampling_weight),
      sampled = as.integer(sampled),
      sample_county = county,
      sample_zone = zone,
      sample_round = as.character(round)
    )
}
