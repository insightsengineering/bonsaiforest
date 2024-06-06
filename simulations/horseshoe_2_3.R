# Sub-scenario 2_3:
# Only the single subgrouping variable with 3 level in which heterogeneity
# is observed is included in the global model (1 subgrouping variable, 3 subgroups).
cov1 <- "x_4"
horseshoe_1 <- get_horseshoe_method(
  subgr = cov1,
  covars = cov1,
  estimator = "horseshoe_1"
)

# Analysis of a single scenario.
horseshoe_1_analysis <- fun_analysis(horseshoe_1)

# Results only for scenario 2.
horseshoe_2_3_results <- compute_results(
  scenarios[2],
  scenario_no = "2",
  analyze = horseshoe_1_analysis,
  cache = "results/horseshoe_2_3.rds"
)
