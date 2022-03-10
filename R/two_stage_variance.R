two_stage_variance = function(y, psu_index, psu_probability, psu_size, ssu_probability, joint_probability) {
  between_psu_variance(y, psu_index, psu_probability, ssu_probability, joint_probability) +
    within_psu_variance(y, psu_index, psu_size, psu_probability)
}

between_psu_variance = function(y, psu_index, psu_probability, ssu_probability, joint_probability) {
  y_over_pi_ssu = y / ssu_probability

  psu_index_factor = factor(psu_index, levels = unique(psu_index), ordered = TRUE)

  x_check = purrr::map2_dbl(
    split(y_over_pi_ssu, psu_index_factor), psu_probability,
    ~ sum(.x) / .y
  )

  d_check = survey:::pi2Dcheck(joint_probability)

  Matrix::crossprod(x_check, Matrix::crossprod(d_check, x_check))[1, 1]
}

within_psu_variance = function(y, psu_index, psu_size, psu_probability) {
  psu_index_factor = factor(psu_index, levels = unique(psu_index), ordered = TRUE)

  intra_psu_variance = purrr::map2_dbl(
    split(y, psu_index_factor), psu_size,
    intra_psu_variance
  )

  sum(intra_psu_variance / psu_probability)
}

intra_psu_variance = function(y, psu_size) {
  n = length(y)

  ind_prob = n / psu_size

  x_check = y / ind_prob

  joint_prob = (n * (n - 1)) / (psu_size * (psu_size - 1))

  joint_prob_matrix = matrix(rep(joint_prob, n^2), nrow = n)
  diag(joint_prob_matrix) = ind_prob

  d_check = survey:::pi2Dcheck(joint_prob_matrix)

  Matrix::crossprod(x_check, Matrix::crossprod(d_check, x_check))[1,1]
}
