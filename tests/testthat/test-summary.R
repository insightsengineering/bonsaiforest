test_that("Summary outputs the right element for naivepop survival", {
  result <- summary(naivepop_fit_surv)
  expected <- exp(coef(naivepop_fit_surv$fit))
  expect_equal(result, expected)
})

test_that("Summary outputs the right element for naivepop binary", {
  result <- summary(naivepop_fit_bin)
  expected <- coef(naivepop_fit_bin$fit)[2]
  expect_equal(result, expected)
})

test_that("Summary outputs the right element for naive survival", {
  result <- summary(naive_fit_surv)
  estim <- naive_fit_surv$estimates
  estim$trt.effect <- exp(estim$estimate)
  estim$trt.conf.low <- exp(estim$estimate - stats::qnorm(0.975) * estim$std.error)
  estim$trt.conf.high <- exp(estim$estimate + stats::qnorm(0.975) * estim$std.error)
  expected <- estim[, c(1, 7:9)]
  expect_equal(result, expected)
})

test_that("Summary outputs the right element for naive binary", {
  result <- summary(naive_fit_bin)
  estim <- naive_fit_bin$estimates
  estim$trt.effect <- estim$estimate
  estim$trt.conf.low <- estim$estimate - stats::qnorm(0.975) * estim$std.error
  estim$trt.conf.high <- estim$estimate + stats::qnorm(0.975) * estim$std.error
  expected <- estim[, c(1, 7:9)]
  expect_equal(result, expected)
})
