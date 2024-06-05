# Method for a single data set.
subgroup_method <- function(df, simul_no) {
  assert_data_frame(df)
  assert_count(simul_no)
  df$arm <- factor(df$arm)
  model <- naive(
    resp = "tt_pfs",
    trt = "arm",
    subgr = c("x_1", "x_2", "x_3", "x_4", "x_5", "x_6", "x_7", "x_8", "x_9", "x_10"),
    data = df,
    resptype = "survival",
    status = "ev_pfs"
  )
  s <- summary(model)
  with(
    s$estimates,
    data.frame(
      simul_no = simul_no,
      estimator = "subgroup",
      subgroup = sanitize_subgroups(subgroup),
      estimate_ahr = trt.estimate,
      estimate_log_ahr = log(trt.estimate),
      lower_ci_ahr = trt.low,
      upper_ci_ahr = trt.high
    )
  )
}

# Analysis of a single scenario.
subgroup_analysis <- fun_analysis(subgroup_method)

# Results across all scenarios.
subgroup_results <- compute_results(
  scenarios,
  analyze = subgroup_analysis,
  cache = "results/subgroup.rds"
)
