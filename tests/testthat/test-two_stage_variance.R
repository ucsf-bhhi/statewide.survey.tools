y = c(
  5, 4.5, 5.5, 5,
  2, 4, 3, 3.5,
  5, 3, 4, 2,
  3.5, 4, 1, 6,
  2, 1.5, 1.5, 3
)

psu_index = c(
  "4", "4", "4", "4",
  "10", "10", "10", "10",
  "1", "1", "1", "1",
  "9", "9", "9", "9",
  "14", "14", "14", "14"
)

psu_probability = c(0.17002, 0.26275, 0.34003, 0.41731, 0.7728)

psu_size = c(22, 34, 44, 54, 100)

ssu_probability = c(
  0.181818181818182, 0.181818181818182, 0.181818181818182, 0.181818181818182,
  0.117647058823529, 0.117647058823529, 0.117647058823529, 0.117647058823529,
  0.090909090909091, 0.090909090909091, 0.090909090909091, 0.090909090909091,
  0.074074074074074, 0.074074074074074, 0.074074074074074, 0.074074074074074,
  0.04, 0.04, 0.04, 0.04
)

joint_probability = matrix(
  c(
    0.17002, 0.03726, 0.04822, 0.05482, 0.11782,
    0.03726, 0.26275, 0.0769, 0.08722, 0.18341,
    0.04822, 0.0769, 0.34003, 0.11647, 0.23922,
    0.05482, 0.08722, 0.11647, 0.41731, 0.31248,
    0.11782, 0.18341, 0.23922, 0.31248, 0.7728
  ),
  nrow = 5,
  ncol = 5
)

test_that("two stage variance works", {
  expect_equal(
    two_stage_variance(
      y, psu_index, psu_probability,
      psu_size, ssu_probability, joint_probability
    ),
    17414.4610
  )
})

test_that("between psu variance works", {
  expect_equal(
    between_psu_variance(
      y, psu_index, psu_probability, ssu_probability, joint_probability
    ),
    6059.5988
  )
})

test_that("within psu variance works", {
  expect_equal(
    within_psu_variance(y, psu_index, psu_size, psu_probability),
    11354.8622
  )
})

test_that("intra psu variance works", {
  expect_equal(
    intra_psu_variance(y[1:4], psu_size[1]),
    16.5
  )
})
