# Method for a single data set.
lasso_method <- function(df) {
  elastic_method(df, alpha = 1)
}

# Analysis of a single scenario.
lasso_analysis <- function(scenario) {
  assert_list(scenario)
  results <- t(sapply(scenario, lasso_method))
}

# Results across all scenarios.
lasso_file <- "results/lasso.rds"
lasso_results <- if (file.exists(lasso_file)) {
  readRDS(lasso_file)
} else {
  res <- mclapply(
    scenarios,
    FUN = function(x) lasso_analysis(x$scenario),
    mc.cores = availableCores()
  )
  saveRDS(res, file = lasso_file)
  res
}
