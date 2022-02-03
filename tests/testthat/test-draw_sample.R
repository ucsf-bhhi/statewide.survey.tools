test_sampling_weights = readr::read_rds("test_data/test_sampling_weights.rds")

test_that("no strata works", {
  withr::local_seed(510)

  test_n = 10

  test_sample = draw_sample(test_sampling_weights, sampling_weight, test_n)

  # make sure sample has same number of rows as venue data
  expect_equal(nrow(test_sample), nrow(test_sampling_weights))
  # make sure the requested number of sites were selected
  expect_equal(sum(test_sample$sampled), test_n)

  # final check to just make sure it does the same thing each time
  expect_snapshot_value(test_sample, style = "json2")
})

test_that("stratified sample works", {
  withr::local_seed(510)

  test_strata = dplyr::tribble(
    ~ zone, ~ site_category, ~ n,
    "Eastern Placer", "Emergency Shelter", 1,
    "Central Placer", "Emergency Shelter", 1,
    "Central Placer", "Non Shelter Venue", 1,
    "South Placer", "Encampment", 3,
    "South Placer", "Non Shelter Venue", 2
  )

  test_sample = draw_sample(
    test_sampling_weights, sampling_weight, test_strata
  )

  # make sure sample has same number of rows as venue data
  expect_equal(nrow(test_sample), nrow(test_sampling_weights))
  # make sure the requested number of sites were selected
  expect_equal(sum(test_sample$sampled), sum(test_strata$n))

  # make sure the requested number of sites per strata were selected
  purrr::pwalk(test_strata, function(zone_s, site_category_s, n) {
    expect_equal(
      test_sample %>%
        dplyr::filter(zone == zone_s, site_category == site_category_s) %>%
        dplyr::pull(sampled) %>%
        sum()
      ,
      n
    )
  })

  # final check to just make sure it does the same thing each time
  expect_snapshot_value(test_sample, style = "json2")
})
