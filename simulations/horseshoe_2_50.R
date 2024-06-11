# Sub-scenario 2_50:
# Original scenario with an additional 10 subgrouping variables simulated using the same
# data-generating model as for the first 10 subgrouping variables,
# i.e. using scenario2_20 as the data sets and the horseshoe_20 model.

cov20 <- c(
  "x_1", "x_2", "x_3", "x_4", "x_5", "x_6", "x_7", "x_8", "x_9", "x_10",
  "x_11", "x_12", "x_13", "x_14", "x_15", "x_16", "x_17", "x_18", "x_19", "x_20"
)
horseshoe_20 <- get_horseshoe_method(
  subgr = cov20,
  covars = cov20,
  estimator = "horseshoe_20"
)

# Analysis of a single scenario.
horseshoe_20_analysis <- fun_analysis(horseshoe_20)

# Results only for scenario 2_20.
horseshoe_2_50_results <- compute_results(
  scenario_2_20,
  # Note: We call it 2 here, because we need to join in with the properties later.
  scenario_no = "2",
  analyze = horseshoe_20_analysis,
  cache = "results/horseshoe_2_50.rds"
)
horseshoe_2_50_results
