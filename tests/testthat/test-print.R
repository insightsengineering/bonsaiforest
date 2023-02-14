test_that("print.summary.horseshoe outputs the right element", {
  result <- print(summary(horseshoe_fit_bin))
  posterior <- trt_horseshoe(horseshoe_fit_bin)
  summary_post <- data.frame(
    subgroup = horseshoe_fit_surv$subgr_names,
    trt.estimate = c(
      -0.4633, -0.467, -0.4665, -0.4646
    ),
    trt.low = c(
      -0.7766, -0.7841, -0.7985, -0.7675
    ),
    trt.high = c(
      -0.1563, -0.1635, -0.1476, -0.162
    )
  )
  expected <- list(
    posterior = posterior,
    summary_post = summary_post
  )
  class(expected) <- "summary.horseshoe"
  expect_equal(result, expected, tolerance = 0.001)
})
