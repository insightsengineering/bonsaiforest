# Method for a single data set.
horseshoe_method <- function(df) {
  df$arm <- factor(df$arm)
  model <- suppressWarnings(horseshoe(
    resp = "tt_pfs",
    trt = "arm",
    subgr = c("x_1", "x_2", "x_3", "x_4", "x_5", "x_6", "x_7", "x_8", "x_9", "x_10"),
    covars = c("x_1", "x_2", "x_3", "x_4", "x_5", "x_6", "x_7", "x_8", "x_9", "x_10"),
    data = df,
    resptype = "survival",
    status = "ev_pfs",
    chains = 1,
    seed = 0,
    iter = 10, # todo increase once format is clear.
    backend = "cmdstanr", # Because this enables caching of the compiled Stan binary.
    silent = 2 # Suppress all messages.
  ))
  summary(model)
}

# Analysis of a single scenario.
horseshoe_analysis <- function(scenario) {
  assert_list(scenario)
  results <- lapply(scenario, horseshoe_method)
}

# Results across all scenarios.
horseshoe_file <- "results/horseshoe.rds"
horseshoe_results <- if (file.exists(horseshoe_file)) {
  readRDS(horseshoe_file)
} else {
  # Tried but does not suppress warnings.
  # sink(stdout(), type = "message") 
  res <- mclapply(
    scenarios,
    FUN = function(x) horseshoe_analysis(x$scenario),
    mc.cores = availableCores()
  )
  # sink(NULL, type = "message")
  saveRDS(res, file = horseshoe_file)
  res
}
# todo: modify results format once target is clear
