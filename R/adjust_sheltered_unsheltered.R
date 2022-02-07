#' Adjust PEH Counts for Sheltered/Unsheltered Share
#'
#' Makes the adjustment to sampling weights so that the weighted average share
#' of residential venue (ie. emergency shelters or encampments) PEH who are
#' sheltered matches a target (usually from PIT counts). It's designed for use
#' within a [dplyr::mutate()] pipeline when calculating the sampling weights.
#'
#' @param peh_count A numeric vector of venue PEH counts.
#' @param site_category A character vector of venue site categories.
#' @param sheltered_share The target sheltered share as a single numeric.
#'
#' @return A vector of adjusted PEH counts.
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
#'     )
#' }
adjust_sheltered_unsheltered = function(
  peh_count, site_category, sheltered_share
) {
  # check inputs
  assertthat::assert_that(is.numeric(peh_count))
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
    peh_count
  )

  # calculate the adjuster
  adjuster = calculate_adjuster(sheltered_share, observed_sheltered_share)

  # apply the adjuster
  purrr::map2_dbl(sheltered_indicator, peh_count, adjust_count, adjuster)
}

#' Create Sheltered or Unsheltered Indicator
#'
#' Based on the site category determine whether a venue counts as sheltered or
#' unsheltered (or neither).
#'
#' @param site_category Character vector with site categories.
#' @param sheltered_categories Character vector with the site categories that
#'   represent sheltered venues.
#' @param unsheltered_categories Character vector with the site categories that
#'   represent unsheltered venues.
#'
#' @return A character vector with either "Sheltered", "Unsheltered, or NA.
#' @export
#'
#' @examples
#' site_category = c(
#'   "Emergency Shelter", "Encampment",
#'   "Non Shelter Venue", "Hotspot"
#' )
#' sheltered_or_unsheltered(site_category)
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

#' Calculate the Sheltered Share in Venue Data
#'
#' Calculate the share of PEH who are sheltered in the venue data. The sheltered
#' share is: `sheltered PEH / (sheltered PEH + unsheltered PEH)`.
#'
#' @param sheltered_indicator A character vector/variable with the
#'   sheltered/unsheltered indicator.
#' @param peh_count A numeric vector/variable with the PEH estimates.
#'
#' @export
#'
#' @examples
#' sheltered_indicator = c("Sheltered", "Unsheltered", NA)
#' peh_count = c(5, 10, 20)
#' observed_sheltered_share(sheltered_indicator, peh_count)
observed_sheltered_share = function(sheltered_indicator, peh_count) {
  sheltered = sum(
    peh_count[sheltered_indicator == "Sheltered"],
    na.rm = TRUE
    )
  sheltered_or_unsheltered = sum(peh_count[!is.na(sheltered_indicator)])

  return(sheltered / sheltered_or_unsheltered)
}

calculate_adjuster = function(sheltered_share, observed_sheltered_share) {
  sheltered_share / observed_sheltered_share
}

adjust_count = function(sheltered_indicator, peh_count, adjuster) {
  dplyr::case_when(
    sheltered_indicator == "Sheltered" ~ peh_count * adjuster,
    sheltered_indicator == "Unsheltered" ~ peh_count * 1 / adjuster,
    TRUE ~ as.numeric(peh_count)
  )
}
