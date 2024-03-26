test_that("elastic_net outputs the right elements for survival", {
  set.seed(0)
  result <- elastic_net(
    "tt_pfs", "arm", c("x_1", "x_3"),
    c("x_1", "x_2", "x_3"), example_data,
    "survival", 1, "ev_pfs"
  )
  result[[1]] <- coef(result$fit)
  prep_data <- preprocess(
    "arm", c("x_1", "x_3"), c("x_1", "x_2", "x_3"),
    example_data
  )
  penalty_factor <- c(rep(0, 5), rep(1, 6))
  design_matrix <- cbind(prep_data$design_main, prep_data$design_ia)
  set.seed(0)
  fit_glmnet <- glmnet::cv.glmnet(design_matrix,
    survival::Surv(
      example_data$tt_pfs,
      example_data$ev_pfs
    ),
    family = "cox",
    penalty.factor = penalty_factor,
    alpha = 1
  )
  y <- as.data.frame(cbind(example_data$tt_pfs, example_data$ev_pfs))
  colnames(y) <- c("resp", "status")
  expected <- list(
    fit = coef(fit_glmnet),
    model = "elastic_net",
    resptype = "survival",
    data = example_data,
    alpha = 1,
    design_matrix = design_matrix,
    design_dummy = prep_data$design_dummy,
    y = y,
    subgr_names = prep_data$subgr_names
  )
  class(expected) <- c("bonsaiforest", "elastic_net")
  expect_equal(result, expected)
})


test_that("elastic_net outputs the right elements for binary", {
  set.seed(0)
  result <- elastic_net(
    "ev_pfs", "arm", c("x_1", "x_3"),
    c("x_1", "x_2", "x_3"), example_data,
    "binary", 0
  )
  result[[1]] <- coef(result$fit)
  prep_data <- preprocess(
    "arm", c("x_1", "x_3"), c("x_1", "x_2", "x_3"),
    example_data
  )
  penalty_factor <- c(rep(0, 4), rep(1, 6))
  design_matrix <- cbind(prep_data$design_main[, -1], prep_data$design_ia)
  set.seed(0)
  fit_glmnet <- glmnet::cv.glmnet(design_matrix,
    example_data$ev_pfs,
    family = "binomial",
    penalty.factor = penalty_factor,
    alpha = 0
  )
  y <- example_data$ev_pfs
  expected <- list(
    fit = coef(fit_glmnet),
    model = "elastic_net",
    resptype = "binary",
    data = example_data,
    alpha = 0,
    design_matrix = design_matrix,
    design_dummy = prep_data$design_dummy,
    y = y,
    subgr_names = prep_data$subgr_names
  )
  class(expected) <- c("bonsaiforest", "elastic_net")
  expect_equal(result, expected)
})
