test_that("trt_horseshoe outputs the right element for survival", {
  result <- trt_horseshoe(horseshoe_fit_surv)[, c(1, 1800)]
  expect_data_frame(result, ncols = 2L, nrows = 4L)
  expect_named(result, c("subgroup", "trt.estimate.1799"))
  expect_identical(result$subgroup, horseshoe_fit_surv$subgr_names)
  expect_numeric(result$trt.estimate.1799, finite = TRUE)
})

test_that("trt_horseshoe outputs the right element for binary", {
  result <- trt_horseshoe(horseshoe_fit_bin)[, c(1, 100)]
  expect_data_frame(result, ncols = 2L, nrows = 4L)
  expect_named(result, c("subgroup", "trt.estimate.99"))
  expect_identical(result$subgroup, horseshoe_fit_bin$subgr_names)
  expect_numeric(result$trt.estimate.99, finite = TRUE)
})
