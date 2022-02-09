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
#' @param size_category Name of the column with the venue size category.
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
#'     sample = final_sample,
#'     venue_id = id,
#'     sampling_weight = sampling_weight,
#'     sampled_indicator = sampled,
#'     size_category = size_category,
#'     county = "Sonoma",
#'     zone = "Santa Rosa",
#'     round = 2
#'   )
#' }
prepare_sample_for_airtable = function(
  sample,
  venue_id,
  sampling_weight,
  sampled_indicator,
  size_category,
  county,
  zone,
  round
) {
  sample %>%
    dplyr::mutate(
      Venue = as.list({{ venue_id }}),
      sampling_weight = as.numeric(sampling_weight),
      sampled = as.integer({{ sampled_indicator }}),
      sample_county = county,
      sample_zone = zone,
      sample_round = as.character(round)
    ) %>%
    dplyr::select(
      Venue, sampling_weight, sampled,
      sample_county, sample_zone, sample_round
    )
}
