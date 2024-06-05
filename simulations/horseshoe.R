# Method for a single data set.
horseshoe_method <- function(df, simul_no) {
  assert_data_frame(df)
  assert_count(simul_no)
  df$arm <- factor(df$arm)
  model <- bonsaiforest::horseshoe(
    resp = "tt_pfs",
    trt = "arm",
    subgr = c("x_1", "x_2", "x_3", "x_4", "x_5", "x_6", "x_7", "x_8", "x_9", "x_10"),
    covars = c("x_1", "x_2", "x_3", "x_4", "x_5", "x_6", "x_7", "x_8", "x_9", "x_10"),
    data = df,
    resptype = "survival",
    status = "ev_pfs",
    chains = 1,
    iter = 2000,
    warmup = 1000,
    control = list(adapt_delta = 0.95),
    seed = 0,
    backend = "cmdstanr", # Because this enables caching of the compiled Stan binary.
    silent = 2 # Suppress all messages.
  )
  s <- summary(model)
  with(
    s$estimates,
    data.frame(
      simul_no = simul_no,
      estimator = "horseshoe",
      subgroup = sanitize_subgroups(subgroup),
      estimate_ahr = trt.estimate,
      estimate_log_ahr = log(trt.estimate),
      lower_ci_ahr = trt.low,
      upper_ci_ahr = trt.high
    )
  )
}

# Analysis of a single scenario.
horseshoe_analysis <- fun_analysis(horseshoe_method)

# Results across all scenarios.
horseshoe_results <- compute_results(
  scenarios,
  analyze = horseshoe_analysis,
  cache = "results/horseshoe.rds"
)
