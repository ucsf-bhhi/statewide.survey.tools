test_that("adjust_new_sites works", {
  sampling_weights = c(1, 3.3, 4, 5, 6.1)
  samples = list(c("xyz", "zyx"), NULL, NULL, "xyz", "zyx")

  expect_equal(
    adjust_new_sites(sampling_weights, samples, 2),
    c(1.0, 6.6, 8.0, 5.0, 6.1)
  )
})
