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

test_that("simul_covariates optionally returns arm as factor", {
  set.seed(123)
  result <- expect_silent(simul_covariates(n = 5, p_catvar = 23, arm_factor = TRUE))
  expect_factor(result$arm, levels = c("0", "1"))
})

# simul_pfs ----

test_that("simul_pfs works as expected", {
  set.seed(123)
  result <- expect_silent(simul_pfs(
    lp_aft = rnorm(100),
    sigma_aft = 1,
    recr_duration = 0.2,
    rate_cens = 2,
    n_events = 20
  ))
  expect_data_frame(result, nrows = 100, ncols = 2)
  expect_named(result, c("tt_pfs", "ev_pfs"))
  expect_numeric(result$tt_pfs, lower = .Machine$double.xmin)
  expect_numeric(result$ev_pfs)
  expect_true(all(result$ev_pfs %in% c(0, 1)))
})

test_that("simul_pfs adds the uncensored PFS when requested", {
  set.seed(123)
  result <- expect_silent(simul_pfs(
    lp_aft = rnorm(100),
    sigma_aft = 1,
    recr_duration = 0.2,
    rate_cens = 2,
    n_events = 20,
    add_uncensored_pfs = TRUE
  ))
  expect_data_frame(result, nrows = 100, ncols = 3)
  expect_named(result, c("tt_pfs", "ev_pfs", "tt_pfs_uncens"))
  expect_numeric(result$tt_pfs, lower = .Machine$double.xmin)
  expect_numeric(result$ev_pfs)
  expect_true(all(result$ev_pfs %in% c(0, 1)))
  expect_numeric(result$tt_pfs_uncens)
  expect_true(all(result$tt_pfs_uncens >= result$tt_pfs - .Machine$double.eps))
})

# simul_data ----

test_that("simul_data works as expected", {
  set.seed(321)
  result <- expect_silent(simul_data(
    n = 100,
    coefs = c(arm1 = 1),
    sigma_aft = 1,
    recr_duration = 0.2,
    rate_cens = 2,
    n_events = 20
  ))
  expect_data_frame(result, nrows = 100)
  cols <- c(
    "id", "arm", "x_1", "x_2", "x_3", "x_4", "x_5", "x_6", "x_7",
    "x_8", "x_9", "x_10", "tt_pfs", "ev_pfs"
  )
  expect_named(result, cols)
  expect_factor(result$arm, levels = c("0", "1"))
})

test_that("simul_data works as expected with interactions between 1st and 2nd covariate", {
  set.seed(321)
  result <- expect_silent(simul_data(
    n = 100,
    coefs = c(arm1 = 0.5),
    sigma_aft = 1,
    recr_duration = 0.5,
    rate_cens = 2,
    n_events = 20,
    add_interaction = TRUE
  ))
  expect_data_frame(result, nrows = 100)
  cols <- c(
    "id", "arm", "x_1", "x_2", "x_3", "x_4", "x_5", "x_6", "x_7",
    "x_8", "x_9", "x_10", "tt_pfs", "ev_pfs"
  )
  expect_named(result, cols)
})
