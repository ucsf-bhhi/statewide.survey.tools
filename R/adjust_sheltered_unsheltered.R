#' Adjust PEH Counts for Sheltered/Unsheltered Share
#'
#' Makes the adjustment to sampling weights so that the weighted average share
#' of residential venue (ie. emergency shelters or encampments) PEH who are
#' sheltered matches a target (usually from PIT counts). It's designed for use
#' within a [dplyr::mutate()] pipeline when calculating the sampling weights or
#' the component functions (listed below) can be used to customize the
#' adjustment.
#'
#' @param sampling_weight A numeric vector/variable with the current sampling
#'   weights.
#' @param site_category A character vector/variable of venue site categories.
#' @param sheltered_share The target sheltered share as a single numeric.
#'
#' @return A vector of adjusted sampling weights.
#' @export
#'
#' @examples
#' \dontrun{
#'   venue_data %>%
#'     mutate(
#'       sampling_weight = adjust_sheltered_unsheltered(
#'         final_peh_estimate,
#'         site_category,
#'         0.45
#'       )
#'     )}
#'
adjust_sheltered_unsheltered = function(
  sampling_weight, site_category, sheltered_share
) {
  # check inputs
  assertthat::assert_that(is.numeric(sampling_weight))
  assertthat::assert_that(is.character(site_category))
  assertthat::assert_that(
    any(c("Emergency Shelter", "Encampment") %in% unique(site_category)),
    msg = "'site_category' doesn't have any sheltered or unsheltered venues.
    Is this the correct variable?"
  )
  assertthat::assert_that(assertthat::is.number(sheltered_share))
  assertthat::assert_that(
    sheltered_share >= 0 & sheltered_share <= 1,
    msg = "'sheltered_share' must be between 0 and 1."
  )

  # turn site_category into a sheltered/unsheltered indicator
  sheltered_indicator = sheltered_or_unsheltered(site_category)

  # calculate observed sheltered share in the venue data
  observed_sheltered_share = observed_sheltered_share(
    sheltered_indicator,
    sampling_weight
  )

  # calculate the adjuster
  adjuster = calculate_adjuster(sheltered_share, observed_sheltered_share)

  # apply the adjuster
  adjust_count(sheltered_indicator, sampling_weight, adjuster)
}

#' @describeIn adjust_sheltered_unsheltered Create Sheltered or Unsheltered
#'   Indicator
#'
#'   Based on the site category determine whether a venue counts as sheltered or
#'   unsheltered (or neither). Returns a character vector with either
#'   "Sheltered", "Unsheltered, or NA.
#'
#' @param sheltered_categories Character vector with the site categories that
#'   represent sheltered venues.
#' @param unsheltered_categories Character vector with the site categories that
#'   represent unsheltered venues.
#'
#' @export
#'
#' @examples
#' site_category = c(
#'   "Emergency Shelter", "Encampment",
#'   "Non Shelter Venue", "Hotspot"
#' )
#' sheltered_indicator = sheltered_or_unsheltered(site_category)
#'
sheltered_or_unsheltered = function(
  site_category,
  sheltered_categories = c("Emergency Shelter"),
  unsheltered_categories = c("Encampment")
) {
  dplyr::case_when(
    site_category %in% sheltered_categories ~ "Sheltered",
    site_category %in% unsheltered_categories ~ "Unsheltered",
    TRUE ~ NA_character_
  )
}

#' @describeIn adjust_sheltered_unsheltered Calculate the Sheltered Share in
#'   Venue Data
#'
#'   Calculate the share of PEH who are sheltered in the venue data. The
#'   sheltered share is: `sheltered PEH / (sheltered PEH + unsheltered PEH)`.
#'
#' @param sheltered_indicator A character vector/variable with the
#'   sheltered/unsheltered indicator.
#'
#' @export
#'
#' @examples
#' sampling_weight = c(5, 10, 20, 15)
#' observed_sheltered_share(sheltered_indicator, sampling_weight)
#'
observed_sheltered_share = function(sheltered_indicator, sampling_weight) {
  sheltered = sum(
    sampling_weight[sheltered_indicator == "Sheltered"],
    na.rm = TRUE
    )
  sheltered_or_unsheltered = sum(sampling_weight[!is.na(sheltered_indicator)])

  return(sheltered / sheltered_or_unsheltered)
}

calculate_adjuster = function(sheltered_share, observed_sheltered_share) {
  sheltered_share / observed_sheltered_share
}

#' @describeIn adjust_sheltered_unsheltered Make Sheltered/Unsheltered
#'   Adjustment
#'
#' @param adjuster A numeric value with the adjuster.
#'
#' @export
#'
#' @examples
#' adjust_count(sheltered_indicator, sampling_weight, 0.9)
adjust_count = function(sheltered_indicator, sampling_weight, adjuster) {
  dplyr::case_when(
    sheltered_indicator == "Sheltered" ~ sampling_weight * adjuster,
    sheltered_indicator == "Unsheltered" ~ sampling_weight * 1 / adjuster,
    TRUE ~ as.numeric(sampling_weight)
  )
}
