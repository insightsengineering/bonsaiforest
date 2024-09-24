test_that("horseshoe outputs the right elements for survival", {
  result <- suppressWarnings(horseshoe(
    "tt_pfs",
    "arm",
    c("x_1", "x_3"),
    c("x_1", "x_2", "x_3"),
    example_data,
    "survival", "ev_pfs",
    iter = 20,
    warmup = 10,
    chains = 1,
    seed = 0,
    control = list(adapt_delta = 0.95)
  ))
  result[[1]] <- as.matrix(result$fit$fit)
  prep_data <- preprocess(
    "arm", c("x_1", "x_3"), c("x_1", "x_2", "x_3"),
    example_data
  )
  design_matrix <- cbind(prep_data$design_main, prep_data$design_ia)
  y <- as.data.frame(cbind(example_data$tt_pfs, example_data$ev_pfs))
  colnames(y) <- c("tt_pfs", "ev_pfs")
  data_model <- cbind(design_matrix, y)
  sort_resp <- sort(y[, 1])
  diff_resp <- min(sort_resp - c(0, sort_resp[-length(y[, 1])]))
  limits_resp <- c(max(min(y[, 1]) - diff_resp, 0), max(y[, 1]) + diff_resp)
  quantiles_resp <- quantile(y[, 1], c(0.25, 0.5, 0.75), names = FALSE)
  form <- paste0(
    "tt_pfs | cens(1 - ev_pfs) + ",
    "bhaz(Boundary.knots = ", deparse(limits_resp), ", ",
    "knots = ", deparse(quantiles_resp), ", intercept = FALSE) ",
    "~ a + b"
  )
  fit_brms <- suppressWarnings(brms::brm(
    brms::bf(form, nl = TRUE) +
      brms::lf(a ~ 0 + arm0 + arm1 + x_1b + x_2b + x_3b) +
      brms::lf(b ~ 0 + x_1a_arm + x_1b_arm + x_2a_arm + x_2b_arm + x_3a_arm + x_3b_arm),
    data = data_model,
    family = brms::brmsfamily("cox"),
    brms::prior(normal(0, 5), class = "b", nlpar = "a") +
      brms::prior(horseshoe(1), class = "b", nlpar = "b"),
    iter = 20, warmup = 10, chains = 1,
    control = list(adapt_delta = 0.95), seed = 0
  ))
  expected <- list(
    fit = as.matrix(fit_brms$fit),
    model = "horseshoe",
    resptype = "survival",
    data = example_data,
    design_matrix = design_matrix,
    design_dummy = prep_data$design_dummy,
    y = y,
    subgr_names = prep_data$subgr_names
  )
  class(expected) <- c("bonsaiforest", "horseshoe")
  expect_equal(result, expected)
})

test_that("horseshoe outputs the right elements for binary", {
  skip_on_cran() # To save time.
  result <- suppressWarnings(horseshoe(
    "ev_pfs",
    "arm",
    c("x_1", "x_3"),
    c("x_1", "x_2", "x_3"),
    example_data,
    "binary",
    iter = 20,
    warmup = 10,
    chains = 1,
    seed = 0,
    control = list(adapt_delta = 0.95)
  ))
  result[[1]] <- as.matrix(result$fit$fit)
  prep_data <- preprocess(
    "arm", c("x_1", "x_3"), c("x_1", "x_2", "x_3"),
    example_data
  )
  design_matrix <- cbind(prep_data$design_main[, -1], prep_data$design_ia)
  y <- as.data.frame(example_data$ev_pfs)
  colnames(y) <- c("ev_pfs")
  data_model <- cbind(design_matrix, y)
  fit_brms <- suppressWarnings(brms::brm(
    brms::bf(ev_pfs ~ a + b, nl = TRUE) +
      brms::lf(a ~ 1 + arm1 + x_1b + x_2b + x_3b) +
      brms::lf(b ~ 0 + x_1a_arm + x_1b_arm + x_2a_arm + x_2b_arm + x_3a_arm + x_3b_arm),
    data = data_model, family = brms::brmsfamily("bernoulli"),
    brms::prior(normal(0, 5), class = "b", nlpar = "a") +
      brms::prior(horseshoe(1), class = "b", nlpar = "b"),
    iter = 20, warmup = 10, chains = 1,
    control = list(adapt_delta = 0.95), seed = 0
  ))
  expected <- list(
    fit = as.matrix(fit_brms$fit),
    model = "horseshoe",
    resptype = "binary",
    data = example_data,
    design_matrix = design_matrix,
    design_dummy = prep_data$design_dummy,
    y = y,
    subgr_names = prep_data$subgr_names
  )
  class(expected) <- c("bonsaiforest", "horseshoe")
  expect_equal(result, expected)
})
