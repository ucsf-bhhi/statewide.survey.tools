#' Draw Venue Sample
#'
#' Draws a weighted, without replacement sample of venues. Supports both
#' stratified and unstratified samples.
#'
#' @param data Data frame with venues.
#' @param weights Name of the weighting variable.
#' @param n Either a single number of venues to select, or a data frame with
#'   stratification plan (see below).
#'
#' @section Stratification Plan:
#' To draw a stratified sample, include a data frame with the stratification
#' plan.
#'
#' The data frame should include a row for each stratum with the values of
#' the stratification variables and the number of venues to sample. The
#' stratification variables must have the same name and coding as in the
#' venue data. The variable with the the number of venues to sample must be
#' named `n`.
#'
#' For example:
#'
#' | **dv_shelter** | **zone**       | **n** |
#' | -------------- | -------------- | ----- |
#' | TRUE           | Eastern Placer | 1     |
#' | TRUE           | Central Placer | 2     |
#' | TRUE           | South Placer   | 3     |
#' | FALSE          | Eastern Placer | 4     |
#' | FALSE          | Central Placer | 5     |
#' | FALSE          | South Placer   | 6     |
#'
#' This can be generated with the following R code:
#' ```
#' tibble::tribble(
#'   ~dv_shelter, ~zone           , ~n,
#'    TRUE      , "Eastern Placer",  1,
#'    TRUE      , "Central Placer",  2,
#'    TRUE      , "South Placer"  ,  3,
#'    FALSE     , "Eastern Placer",  4,
#'    FALSE     , "Central Placer",  5,
#'    FALSE     , "South Placer"  ,  6
#' )
#' ```
#'
#' @return The venue data frame with the variable `sampled` added, which
#'   indicates whether the venue was selected.
#' @export
#'
#' @examples
#' \dontrun{
#'   draw_sample(venues, sampling_weight, 5)
#'
#'   strata = tibble::tribble(
#'     ~zone          , ~n,
#'     "Eastern Placer",  1,
#'     "Central Placer",  3,
#'     "South Placer"  ,  5
#'   )
#'   draw_sample(venues, sampling_weight, strata)
#' }
draw_sample = function(data, weights, n) {
  # make sure the main df doesn't already have a variable called sampled
  if (has_name(data, "sampled"))
    abort("Venue data already has a variable named 'sampled'. Please rename or remove this variable.")

  # make sure that the main df has the weights variable
  weighting_var_name = rlang::as_string(rlang::ensym(weights))
  if (!has_name(data, weighting_var_name)) {
    abort(glue::glue("Venue data is missing the weighting variable: '{weighting_var_name}'."))
  }

  n_int = assertthat::is.count(n)
  n_df = is.data.frame(n)

  if (!(n_int | n_df))
    abort("n must be a single positive integer or a data frame with strata.")
  data = dplyr::mutate(data, .id = dplyr::row_number())

  if (n_df) {
    # make sure the strata df has a column named n
    if (!has_name(n, "n"))
      abort("Strata data frame is missing 'n' column.")

    # make sure the main df has all of the stratification variables
    strata_vars = names(n)[names(n) != "n"]
    purrr::walk(
      strata_vars,
      ~ if ((!has_name(data, .x)))
          abort(
            glue::glue(
              "Venue data is missing the stratification variable: '{.x}'."
            )
          )
    )

    results = vector("list", nrow(n))
    for (i in seq_len(nrow(n))) {
      results[[i]] = dplyr::semi_join(
        data,
        n[i,],
        by = strata_vars
      ) %>%
        dplyr::slice_sample(n = n$n[i], weight_by = {{ weights }}) %>%
        dplyr::pull(.id)
    }
    ids = purrr::flatten_chr(results)
  }

  if (n_int) {
    ids = dplyr::slice_sample(data, n = n, weight_by = {{ weights }}) %>%
      dplyr::pull(.id)
  }

  dplyr::mutate(data, sampled = .id %in% ids) %>%
    dplyr::select(-.id)
}
