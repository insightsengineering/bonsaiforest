# Method for a single data set.
subgroup_method <- function(df) {
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
subgroup_analysis <- function(scenario) {
  assert_list(scenario)
  results <- lapply(scenario, subgroup_method)
}

# Results across all scenarios.
subgroup_file <- "results/subgroup.rds"
subgroup_results <- if (file.exists(subgroup_file)) {
  readRDS(subgroup_file)
} else {
  res <- mclapply(
    scenarios,
    FUN = function(x) subgroup_analysis(x$scenario),
    mc.cores = availableCores()
  )
  saveRDS(res, file = subgroup_file)
  res
}
# todo: modify results format once target is clear
