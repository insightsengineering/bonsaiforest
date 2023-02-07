test_that("lOR_estimation outputs the right element", {
  result <- lOR_estimation(elastic_net_fit_bin$design_matrix,
                           elastic_net_fit_bin$design_dummy,
                           as.matrix(coef(elastic_net_fit_bin$fit,
                                          s = elastic_net_fit_bin$fit$lambda.min)))
  expected <- -0.4632757
  expect_equal(result, expected, tolerance = 0.00001)
})
