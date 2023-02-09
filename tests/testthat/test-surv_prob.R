test_that("surv_prob outputs the right element", {
  mat1 <- matrix(c(1, 2, 3), ncol = 1)
  mat2 <- matrix(c(1, 0, 1), ncol = 1)
  result <- surv_prob(mat1, mat2)
  expected <- list(matrix(c(10.06429, 1, 10.06429), ncol = 1))
  expect_equal(result, expected, tolerance = 0.0001)
})
