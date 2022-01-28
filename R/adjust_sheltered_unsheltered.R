#' Adjust PEH Counts for Sheltered/Unsheltered Share
#'
#' @param peh_count A numeric vector of venue PEH counts.
#' @param site_category A character vector of venue site categories.
#' @param sheltered_share The target sheltered share as a single numeric.
#'
#' @return A vector of adjusted PEH counts.
#' @export
#'
#' @examples
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

sheltered_or_unsheltered = function(site_category) {
  dplyr::case_when(
    site_category == "Emergency Shelter" ~ "Sheltered",
    site_category == "Encampment" ~ "Unsheltered",
    TRUE ~ NA_character_
  )
}

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
    TRUE ~ peh_count
  )
}
