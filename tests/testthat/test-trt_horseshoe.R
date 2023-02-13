test_that("trt_horseshoe outputs the right element for survival", {
  result <- trt_horseshoe(horseshoe_fit_surv)[, c(1, 1800)]
  expected <- data.frame(
    subgroup = horseshoe_fit_surv$subgr_names,
    trt.estimate.1799 = c(
      0.5827, 0.6023, 0.5934, 0.5927
    )
  )
  expect_equal(result, expected, tolerance = 0.001)
})


test_that("trt_horseshoe outputs the right element for binary", {
  result <- trt_horseshoe(horseshoe_fit_bin)[, c(1, 100)]
  expected <- data.frame(
    subgroup = horseshoe_fit_surv$subgr_names,
    trt.estimate.99 = c(
      -0.5327, -0.4943, -0.4568, -0.5533
    )
  )
  expect_equal(result, expected, tolerance = 0.001)
})
