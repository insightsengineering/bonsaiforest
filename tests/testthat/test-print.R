test_that("print.summary.naivepop outputs the right element", {
  result <- print(summary(naivepop_fit_bin))
  estimates <- coef(naivepop_fit_bin$fit)[2]
  expected <- list(
    estimates = estimates,
    resptype = "binary"
  )
  class(expected) <- "summary.naivepop"
  expect_equal(result, expected)
})


test_that("print.summary.naive outputs the right element", {
  result <- print(summary(naive_fit_bin))
  estimates <- data.frame(
    subgroup = c(
      "x_1a", "x_1b", "x_2a", "x_2b", "x_3a", "x_3b", "x_4a",
      "x_4b", "x_4c", "x_5a", "x_5b", "x_5c", "x_5d", "x_6a",
      "x_6b", "x_7a", "x_7b", "x_8a", "x_8b", "x_8c", "x_9a",
      "x_9b", "x_10a", "x_10b", "x_10c"
    ),
    trt.estimate = c(
      -0.4621, -0.4217, -0.3952, -0.4786, -0.7587, -0.3707,
      -0.5144, -0.6337, -0.2031, 0.0098, -1.0469, -0.5333, -0.387,
      -0.3174, -0.5261, -0.7501, -0.285, -0.6142, -0.5684, -0.309,
      -0.8926, -0.3509, -0.3762, -0.4055, -0.5053
    ),
    trt.low = c(
      -0.8808, -0.8259, -0.8551, -0.8537, -1.3794, -0.7006,
      -1.0023, -1.1784, -0.7012, -0.6961, -1.8869, -1.0757,
      -0.8424, -0.8131, -0.8857, -1.2476, -0.6462, -1.2577,
      -1.1143, -0.7175, -1.5942, -0.6724, -1.0306, -0.9238,
      -0.9226
    ),
    trt.high = c(
      -0.0433, -0.0176, 0.0647, -0.1034, -0.138, -0.0407,
      -0.0265, -0.0891, 0.2949, 0.7157, -0.2069, 0.0092,
      0.0683, 0.1782, -0.1665, -0.2526, 0.0763, 0.0293,
      -0.0225, 0.0994, -0.191, -0.0295, 0.2782, 0.1129, -0.088
    )
  )
  expected <- list(
    estimates = estimates,
    resptype = "binary",
    conf = 0.95
  )
  class(expected) <- "summary.naive"
  expect_equal(result, expected, tolerance = 0.001)
})


test_that("print.summary.elastic_net outputs the right element", {
  result <- print(summary(elastic_net_fit_bin))
  estimates <- data.frame(
    subgroup = elastic_net_fit_surv$subgr_names,
    trt.estimate = c(
      -0.463, -0.4641, -0.4641, -0.4627, -0.4641, -0.4656,
      -0.468, -0.4692, -0.4694, -0.4634, -0.4636, -0.4637,
      -0.4642, -0.4655, -0.4641, -0.4649, -0.4634, -0.4634,
      -0.4638, -0.4631, -0.4658, -0.4636, -0.4636, -0.464,
      -0.4645
    )
  )
  expected <- list(
    estimates = estimates,
    resptype = "binary",
    alpha = 1
  )
  class(expected) <- "summary.elastic_net"
  expect_equal(result, expected, tolerance = 0.001)
})


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
    summary_post = summary_post,
    resptype = "binary",
    conf = 0.95
  )
  class(expected) <- "summary.horseshoe"
  expect_equal(result, expected, tolerance = 0.001)
})
