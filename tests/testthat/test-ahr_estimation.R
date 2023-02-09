test_that("ahr_estimation outputs the right element", {
  result <- ahr_estimation(
    elastic_net_surv$design1, elastic_net_surv$dummy1,
    elastic_net_surv$est_coef, elastic_net_surv$h0
  )
  expected <- 0.6500483
  expect_equal(result, expected, tolerance = 0.00001)
})
