test_that("naive outputs the right elements for survival", {
  result <- naive(
    "tt_pfs", "arm", c("x_1", "x_2"),
    example_data, "survival", "ev_pfs"
  )
  stacked_data <- generate_stacked_data(Surv(tt_pfs, ev_pfs) ~ arm,
    ~ x_1 + x_2, example_data,
    resptype = "survival"
  )
  list_subg <- split(stacked_data, ~subgroup)
  fit <- lapply(list_subg, FUN = function(data) {
    survival::coxph(survival::Surv(time, status) ~ arm, data = data)
  })
  names(fit) <- gsub("\\.", "", names(fit))
  naive_estimates <- cbind(
    subgroup = names(fit),
    do.call(rbind.data.frame, lapply(fit, broom::tidy))
  )
  expected <- list(
    fit = fit,
    estimates = naive_estimates,
    model = "naive",
    resptype = "survival",
    data = example_data
  )
  class(expected) <- c("bonsaiforest", "naive")
  expect_equal(result, expected, ignore_attr = TRUE)
})

test_that("naive outputs the right elements for binary", {
  result <- naive(
    "ev_pfs", "arm", c("x_1", "x_2"),
    example_data, "binary"
  )
  stacked_data <- generate_stacked_data(ev_pfs ~ arm,
    ~ x_1 + x_2, example_data,
    resptype = "binary"
  )
  list_subg <- split(stacked_data, ~subgroup)
  fit <- lapply(list_subg, FUN = function(data) {
    stats::glm(y ~ arm, data = data, family = "binomial")
  })
  names(fit) <- gsub("\\.", "", names(fit))
  naive_estimates <- cbind(
    subgroup = names(fit),
    do.call(
      rbind.data.frame,
      lapply(fit, broom::tidy)
    )[c(2, 4, 6, 8), ]
  )
  expected <- list(
    fit = fit,
    estimates = naive_estimates,
    model = "naive",
    resptype = "binary",
    data = example_data
  )
  class(expected) <- c("bonsaiforest", "naive")
  expect_equal(result, expected, ignore_attr = TRUE)
})
