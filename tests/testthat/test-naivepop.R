test_that("naivepop produces expected coefficients of the model", {
  result <- stats::coef(naivepop("tt_pfs", "arm", example_data, "survival", "ev_pfs")$fit)
  expected <- stats::coef(survival::coxph(survival::Surv(tt_pfs, ev_pfs) ~ arm,
    data = example_data
  ))
  expect_equal(result, expected)
})


test_that("naivepop outputs the right elements", {
  result <- naivepop("tt_pfs", "arm", example_data, "survival", "ev_pfs")[-1]
  expected <- list(model = "naive_pop", resptype = "survival", data = example_data)
  expect_equal(result, expected)
})
