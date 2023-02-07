test_that("lor_estimation outputs the right element", {
  result <- lor_estimation(design_matrix_bin1, design_dummy_bin1,
                           est_coef_bin1)
  expected <- -0.4630356
  expect_equal(result, expected, tolerance = 0.00001)
})
