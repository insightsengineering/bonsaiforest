# Sub-scenario 2_9:
# The first 4 subgrouping variables are included in
# the global model (4 subgrouping variables, 9 subgroups).
cov4 <- c("x_1", "x_2", "x_3", "x_4")
horseshoe_4 <- get_horseshoe_method(
  subgr = cov4,
  covars = cov4,
  estimator = "horseshoe_4"
)

# Analysis of a single scenario.
horseshoe_4_analysis <- fun_analysis(horseshoe_4)

# Results only for scenario 2.
horseshoe_2_9_results <- compute_results(
  scenarios[2],
  scenario_no = "2",
  analyze = horseshoe_4_analysis,
  cache = "results/horseshoe_2_9.rds"
)
