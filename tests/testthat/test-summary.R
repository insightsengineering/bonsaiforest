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
