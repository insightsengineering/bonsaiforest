# Method for a single data set.
lasso_method <- function(df, simul_no) {
  elastic_method(df, simul_no, alpha = 1, estimator = "lasso")
}

# Analysis of a single scenario.
lasso_analysis <- fun_analysis(lasso_method)

# Results across all scenarios.
lasso_results <- compute_results(
  scenarios,
  analyze = lasso_analysis,
  cache = "results/lasso.rds"
)
