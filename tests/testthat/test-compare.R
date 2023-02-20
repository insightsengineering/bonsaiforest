test_that("compare outputs the right element", {
  result <- compare(
    naivepop_fit_surv, naive_fit_surv
  )
  data <- data.frame(summary(naive_fit_surv)$estimates,
    model = rep("Naive", nrow(summary(naive_fit_surv)$estimates))
  )
  data$subgroup <- as.factor(data$subgroup)
  data$model <- as.factor(data$model)
  expected <- list(
    data = data,
    overall_trt = 0.666919,
    resptype = "survival"
  )
  class(expected) <- "compare.data"
  expect_equal(result, expected, tolerance = 0.001)
})
