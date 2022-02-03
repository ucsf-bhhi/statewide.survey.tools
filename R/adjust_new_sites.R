#' Adjust New Sites' Sampling Weights
#'
#' Increases the sampling weights of new venues. New venues are ones have not
#' been eligible to be selected in a previous sample.
#'
#' @param sampling_weight The name of the variable with the sampling weights.
#' @param samples The name of the variable containing the samples the venue has been eligible for.
#' @param new_venue_factor The scaling factor for new venues.
#'
#' @return A vector of adjusted sampling weights.
#' @export
#'
#' @examples
#' \dontrun{
#' venues %>%
#'   mutate(sampling_weight = adjust_new_sites(sampling_weight, samples, 2))
#' }
adjust_new_sites = function(sampling_weight, samples, new_venue_factor) {
  assertthat::assert_that(is.numeric(sampling_weight))

  if (!is.list(samples))
    abort("'samples' is not a list-column. Did you use the correct variable?")

  if (length(sampling_weight) != length(samples))
    abort("'sampling_weight' and 'samples' must be the same length.")

  assertthat::assert_that(assertthat::is.number(new_venue_factor))

  if (new_venue_factor <= 0)
    abort("'new_venue_factor' must be positive.")

  purrr::map2_dbl(sampling_weight, samples, ~ ifelse(is.null(.y), .x * new_venue_factor, .x))
}
