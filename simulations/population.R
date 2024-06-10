# Method for a single data set.
population_method <- function(df, simul_no) {
  assert_data_frame(df)
  assert_count(simul_no)
  df$arm <- factor(df$arm)
  model <- naivepop(
    resp = "tt_pfs",
    trt = "arm",
    data = df,
    resptype = "survival",
    status = "ev_pfs"
  )
  ahr <- as.numeric(summary(model)$estimates)
  conf_int <- exp(confint(model$fit))
  data.frame(
    simul_no = simul_no,
    estimator = "population",
    subgroup = all_subgroups,
    estimate_ahr = ahr,
    estimate_log_ahr = log(ahr),
    lower_ci_ahr = conf_int[1L],
    upper_ci_ahr = conf_int[2L]
  )
}

# Analysis of a single scenario.
population_analysis <- fun_analysis(population_method)

# Results across all scenarios.
population_results <- compute_results(
  scenarios,
  analyze = population_analysis,
  cache = "results/population.rds"
)
