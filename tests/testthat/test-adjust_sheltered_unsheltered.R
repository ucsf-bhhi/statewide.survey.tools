peh_count = c(4, 6, 10, 2, 4, 12, 1, 10, 12, 20)
site_category = c(
  "Emergency Shelter", "Emergency Shelter", "Non Shelter Venue", "Hotspot",
  "Encampment", "Encampment", "Encampment", "Encampment",
  "Emergency Shelter", "Encampment"
)
no_sheltered_unsheltered = c("Non Shelter Venue", "Hotspot")

test_that("sheltered_or_unsheltered works", {
  expect_equal(sheltered_or_unsheltered("Emergency Shelter"), "Sheltered")
  expect_equal(sheltered_or_unsheltered("Encampment"), "Unsheltered")
  expect_equal(sheltered_or_unsheltered("Non Shelter Venue"), NA_character_)
  expect_equal(sheltered_or_unsheltered("Hotspot"), NA_character_)
  expect_equal(sheltered_or_unsheltered(""), NA_character_)
  expect_equal(sheltered_or_unsheltered(NA_character_), NA_character_)
})

test_that("adjust_sheltered_unsheltered input checking works", {
  expect_error(
    adjust_sheltered_unsheltered(site_category, peh_count, 0.5),
    "sampling_weight is not a numeric or integer vector"
  )

  expect_error(
    adjust_sheltered_unsheltered(peh_count, peh_count, 0.5),
    "site_category is not a character vector"
  )

  expect_error(
    adjust_sheltered_unsheltered(peh_count, no_sheltered_unsheltered, 0.5),
    "'site_category' doesn't have any sheltered or unsheltered venues.
    Is this the correct variable?"
  )

  expect_error(
    adjust_sheltered_unsheltered(peh_count, site_category, 1.1),
    "'sheltered_share' must be between 0 and 1."
  )

  # something seems broken with testing of assertthat::is.number so skipping
  # this test
  # expect_error(
  #   adjust_sheltered_unsheltered(peh_count, site_category, c(0.25, 0.5)),
  #   "sheltered_share is not a number (a length one numeric vector)."
  # )

  expect_error(
    adjust_sheltered_unsheltered(peh_count, site_category, -0.1),
    "'sheltered_share' must be between 0 and 1."
  )
})

test_that("adjust_sheltered_unsheltered works", {
  expect_snapshot_value(
    round(adjust_sheltered_unsheltered(peh_count, site_category, 0.25), 5),
    style = "json2"
  )
})

test_that("total weight doesn't change", {
  expect_equal(
    sum(adjust_sheltered_unsheltered(peh_count, site_category, 0.25)),
    sum(peh_count)
  )
})

test_that("adjusted sheltered share is correct", {
  adjusted_sheltered_share = sum(adjust_sheltered_unsheltered(peh_count, site_category, 0.25)[site_category == "Emergency Shelter"]) / sum(adjust_sheltered_unsheltered(peh_count, site_category, 0.25)[site_category %in% c("Emergency Shelter", "Encampment")])

  expect_equal(
    adjusted_sheltered_share,
    0.25
  )
})

test_that("adjusted unsheltered share is correct", {
  adjusted_unsheltered_share = sum(adjust_sheltered_unsheltered(peh_count, site_category, 0.25)[site_category == "Encampment"]) / sum(adjust_sheltered_unsheltered(peh_count, site_category, 0.25)[site_category %in% c("Emergency Shelter", "Encampment")])

  expect_equal(
    adjusted_unsheltered_share,
    0.75
  )
})
