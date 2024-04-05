# Method for a single data set.
ridge_method <- function(df) {
  elastic_method(df, alpha = 0)
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
