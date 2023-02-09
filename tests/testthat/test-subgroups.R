test_that("subgroups outputs the right element for survival", {
  result <- subgroups(
    elastic_net_fit_surv, elastic_net_surv$est_coef,
    elastic_net_surv$h0
  )
  expected <- data.frame(
    subgroup = elastic_net_fit_surv$subgr_names,
    trt.estimate = c(
      0.65, 0.6494, 0.6494, 0.6502, 0.6498, 0.649, 0.6474,
      0.6467, 0.6467, 0.6498, 0.6496, 0.6494, 0.649, 0.6486,
      0.6494, 0.6489, 0.6498, 0.6497, 0.6496, 0.6499, 0.6487,
      0.6498, 0.6498, 0.6496, 0.6493
    )
  )
  expect_equal(result, expected, tolerance = 0.001)
})

test_that("subgroups outputs the right element for binary", {
  result <- subgroups(
    elastic_net_fit_bin, est_coef_bin1
  )
  expected <- data.frame(
    subgroup = elastic_net_fit_surv$subgr_names,
    trt.estimate = c(
      -0.463, -0.4641, -0.4641, -0.4627, -0.4641, -0.4656,
      -0.468, -0.4692, -0.4694, -0.4634, -0.4636, -0.4637,
      -0.4642, -0.4655, -0.4641, -0.4649, -0.4634, -0.4634,
      -0.4638, -0.4631, -0.4658, -0.4636, -0.4636, -0.464,
      -0.4645
    )
  )
  expect_equal(result, expected, tolerance = 0.001)
})
