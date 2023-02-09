test_that("survival_curves outputs the right element", {
  mat <- matrix(c(1, 2, 3, 4), nrow = 2)
  h0 <- c(1, 2, 3)
  est_coef <- matrix(c(1, 0), ncol = 1)
  result <- survival_curves(mat, h0, est_coef)
  k <- -exp(matrix(c(1, 2), ncol = 1))
  expected <- surv_prob(k, as.matrix(h0))[[1]]
  expect_equal(result, expected)
})
