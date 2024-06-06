# Method for a single data set.
cov10 <- c("x_1", "x_2", "x_3", "x_4", "x_5", "x_6", "x_7", "x_8", "x_9", "x_10")
horseshoe_method <- get_horseshoe_method(
  subgr = cov10,
  covars = cov10,
  estimator = "horseshoe"
)

# Analysis of a single scenario.
horseshoe_analysis <- fun_analysis(horseshoe_method)

# Results across all scenarios.
horseshoe_results <- compute_results(
  scenarios,
  analyze = horseshoe_analysis,
  cache = "results/horseshoe.rds"
)
