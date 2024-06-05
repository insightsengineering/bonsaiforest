# ahr_estimation ----

test_that("ahr_estimation outputs the right element", {
  result <- ahr_estimation(
    elastic_net_surv$design1, elastic_net_surv$dummy1,
    elastic_net_surv$est_coef, elastic_net_surv$h0
  )
  expected <- 0.6500483
  expect_equal(result, expected, tolerance = 0.00001)
})

# km_fun ----

test_that("km_fun works as expected", {
  result <- km_fun(
    resp = "tt_pfs",
    status = "ev_pfs",
    data = example_data,
    times = seq(from = 0, to = 5, length = 100)
  )
  expect_numeric(result, lower = 0, upper = 1, len = 100)
  expect_true(all(diff(result) <= 0))
})

# ahr_from_km ----

test_that("ahr_from_km works as expected", {
  result <- ahr_from_km("tt_pfs", "arm", example_data, "ev_pfs")
  expect_equal(result, 0.6756848, tolerance = 1e-6)

  result <- ahr_from_km("tt_pfs", "arm", example_data, "ev_pfs", t_quantile = 0.95)
  expect_equal(result, 0.6812255, tolerance = 1e-6)
})
