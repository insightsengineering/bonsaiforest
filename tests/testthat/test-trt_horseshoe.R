test_that("trt_horseshoe outputs the right element for survival", {
  result <- trt_horseshoe(horseshoe_fit_surv)[, c(1, 1800)]
  expected <- data.frame(
    subgroup = horseshoe_fit_surv$subgr_names,
    trt.estimate.1799 = c(
      0.5827, 0.6023, 0.5934, 0.5927, 0.5891,
      0.5936, 0.5901, 0.59, 0.5889, 0.591, 0.5895,
      0.5957, 0.5892, 0.5921, 0.5927, 0.5875, 0.5947,
      0.5918, 0.5933, 0.5932, 0.5942, 0.5927, 0.5932,
      0.5954, 0.5906
    )
  )
  expect_equal(result, expected, tolerance = 0.001)
})


test_that("trt_horseshoe outputs the right element for binary", {
  result <- trt_horseshoe(horseshoe_fit_bin)[, c(1, 100)]
  expected <- data.frame(
    subgroup = horseshoe_fit_surv$subgr_names,
    trt.estimate.99 = c(
      -0.5327, -0.4943, -0.4568, -0.5533, -0.7545, -0.4415,
      -0.589, -0.6568, -0.3344, -0.221, -1.0007, -0.5333,
      -0.4504, -0.5463, -0.4958, -0.4841, -0.5292, -0.4746,
      -0.5434, -0.5126, -0.5534, -0.5012, -0.4544, -0.5266,
      -0.5357
    )
  )
  expect_equal(result, expected, tolerance = 0.001)
})
