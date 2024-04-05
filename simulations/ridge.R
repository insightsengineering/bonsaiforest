# Method for a single data set.
ridge_method <- function(df) {
  df$arm <- factor(df$arm)
  model <- elastic_net(
    resp = "tt_pfs",
    trt = "arm",
    subgr = c("x_1", "x_2", "x_3", "x_4", "x_5", "x_6", "x_7", "x_8", "x_9", "x_10"),
    covars = c("x_1", "x_2", "x_3", "x_4", "x_5", "x_6", "x_7", "x_8", "x_9", "x_10"),
    data = df,
    resptype = "survival",
    alpha = 0,
    status = "ev_pfs"
  )
  s <- summary(model)
  setNames(
    s$estimates$trt.estimate,
    nm = s$estimates$subgroup
  )
}

# Analysis of a single scenario.
ridge_analysis <- function(scenario) {
  assert_list(scenario)
  results <- t(sapply(scenario, ridge_method))
}

# Results across all scenarios.
ridge_file <- "results/ridge.rds"
ridge_results <- if (file.exists(ridge_file)) {
  readRDS(ridge_file)
} else {
  res <- mclapply(
    scenarios,
    FUN = function(x) ridge_analysis(x$scenario),
    mc.cores = availableCores()
  )
  saveRDS(res, file = ridge_file)
  res
}
