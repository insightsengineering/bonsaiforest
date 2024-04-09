# Method for a single data set.
ridge_method <- function(df, simul_no) {
  elastic_method(df, simul_no, alpha = 0, estimator = "ridge")
}

# Analysis of a single scenario.
ridge_analysis <- fun_analysis(ridge_method)

# Results across all scenarios.
ridge_results <- compute_results(
  scenarios,
  analyze = ridge_analysis,
  cache = "results/ridge.rds"
)
