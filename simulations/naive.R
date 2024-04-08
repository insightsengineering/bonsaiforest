# Method for a single data set.
naive_method <- function(df) {
  df$arm <- factor(df$arm)
  model <- naive(
    resp = "tt_pfs",
    trt = "arm",
    subgr = c("x_1", "x_2", "x_3", "x_4", "x_5", "x_6", "x_7", "x_8", "x_9", "x_10"),
    data = df,
    resptype = "survival",
    status = "ev_pfs"
  )
  summary(model)
}

# Analysis of a single scenario.
naive_analysis <- function(scenario) {
  assert_list(scenario)
  results <- lapply(scenario, naive_method)
}

# Results across all scenarios.
naive_file <- "results/naive.rds"
naive_results <- if (file.exists(naive_file)) {
  readRDS(naive_file)
} else {
  res <- mclapply(
    scenarios,
    FUN = function(x) naive_analysis(x$scenario),
    mc.cores = availableCores()
  )
  saveRDS(res, file = naive_file)
  res
}
# todo: modify results format once target is clear
