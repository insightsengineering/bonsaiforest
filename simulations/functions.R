# Sanitize subgroup string format.
sanitize_subgroups <- function(subgroups) {
  assert_character(subgroups)
  gsub(pattern = "^x_", replacement = "S_", x = subgroups)
}

# Elastic Net Method for a single data set.
elastic_method <- function(df, simul_no, alpha, estimator) {
  assert_data_frame(df)
  assert_count(simul_no)
  assert_string(estimator)
  df$arm <- factor(df$arm)
  model <- elastic_net(
    resp = "tt_pfs",
    trt = "arm",
    subgr = c("x_1", "x_2", "x_3", "x_4", "x_5", "x_6", "x_7", "x_8", "x_9", "x_10"),
    covars = c("x_1", "x_2", "x_3", "x_4", "x_5", "x_6", "x_7", "x_8", "x_9", "x_10"),
    data = df,
    resptype = "survival",
    alpha = alpha,
    status = "ev_pfs"
  )
  s <- summary(model)
  with(
    s$estimates,
    data.frame(
      simul_no = simul_no,
      estimator = estimator,
      subgroup = sanitize_subgroups(subgroup),
      estimate_ahr = trt.estimate,
      estimate_log_ahr = log(trt.estimate),
      lower_ci_ahr = NA_real_,
      upper_ci_ahr = NA_real_
    )
  )
}

# Function constructor to return a function that analyses a single scenario with a `fun_method`.
fun_analysis <- function(fun_method) {
  function(scenario, scenario_no) {
    assert_list(scenario)
    assert_count(scenario_no)
    simul_no <- seq_along(scenario)
    results <- Map(f = fun_method, scenario, simul_no)
    results <- do.call(rbind, results)
    results <- cbind(scenario_no = scenario_no, results)
    results
  }
}

# Compute results across all scenarios for a specific method,
# with optional caching.
compute_results <- function(scenarios, analyze, cache = NULL) {
  assert_list(scenarios)
  assert_function(analyze)
  assert_string(cache, null.ok = TRUE)
  scenario_no <- seq_along(scenarios)

  if (!is.null(cache) && file.exists(cache)) {
    readRDS(cache)
  } else {
    res <- parallel::mcmapply(
      FUN = function(x, y) analyze(x$scenario, y),
      x = scenarios,
      y = scenario_no,
      SIMPLIFY = FALSE,
      mc.cores = availableCores(),
      mc.silent = FALSE
    )
    res <- do.call(rbind, res)
    saveRDS(res, file = cache)
    res
  }
}
