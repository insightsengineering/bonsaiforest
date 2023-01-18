test_that("Naivepop produces expected coefficients of the model", {
  dat <- survival::myeloid
  result <- coef(naivepop("futime", "trt", dat, "survival", "death")$fit)
  expected <- coef(coxph(Surv(futime, death) ~ trt, data = dat))
  expect_equal(result, expected)
})


test_that("Naivepop outputs the right elements", {
  dat <- survival::myeloid
  result <- naivepop("futime", "trt", dat, "survival", "death")[-1]
  expected <- list(model = "naive_pop", resptype = "survival", data = dat)
  expect_equal(result, expected)
})
