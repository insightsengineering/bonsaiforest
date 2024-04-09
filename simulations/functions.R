# Elastic Net Method for a single data set.
elastic_method <- function(df, alpha) {
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
  setNames(
    s$estimates$trt.estimate,
    nm = s$estimates$subgroup
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
    res <- mcMap(
      f = function(x, y) analyze(x$scenario, y),
      scenarios,
      scenario_no,
      mc.cores = availableCores()
    )
    res <- do.call(rbind, res)
    saveRDS(res, file = cache)
    res
  }
}
