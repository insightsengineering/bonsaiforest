# cut_norm_quant ----

test_that("cut_norm_quant works as expected", {
  x <- c(-2, -1, 0, 1, 2)
  result <- expect_silent(cut_norm_quant(x = x, prob = c(0.2, 0.5)))
  expected <- factor(c("a", "a", "b", "c", "c"))
  expect_identical(result, expected)
})

# simul_covariates ----

test_that("simul_covariates works as expected with less than 10 covariates", {
  set.seed(123)
  result <- expect_silent(simul_covariates(n = 5, p_catvar = 5, add_contvars = FALSE))
  expect_data_frame(result, nrows = 5, ncols = 5 + 1)
  expect_snapshot(result)
})

test_that("simul_covariates works as expected with more than 10 covariates", {
  set.seed(123)
  result <- expect_silent(simul_covariates(n = 5, p_catvar = 23, add_contvars = FALSE))
  expect_data_frame(result, nrows = 5, ncols = 23 + 1)
  expect_snapshot(result)
})

test_that("simul_covariates works as expected when adding continuous covariates", {
  set.seed(123)
  result <- expect_silent(simul_covariates(n = 5, p_catvar = 23, add_contvars = TRUE))
  expect_data_frame(result, nrows = 5, ncols = 23 + 1 + 23)
  expect_snapshot(result)
})
