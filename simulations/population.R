# Method for a single data set.
population_method <- function(df) {
  df$arm <- factor(df$arm)
  model <- naivepop(
    resp = "tt_pfs",
    trt = "arm",
    data = df,
    resptype = "survival",
    status = "ev_pfs"
  )
  s <- summary(model)
  conf_int <- exp(confint(model$fit))
  c(
    hr = as.numeric(s$estimates),
    lower = conf_int[1L],
    upper = conf_int[2L]
  )
}

# Analysis of a single scenario.
population_analysis <- function(scenario) {
  assert_list(scenario)
  results <- t(sapply(scenario, population_method))
}

# Results across all scenarios.
population_file <- "results/population.rds"
population_results <- if (file.exists(population_file)) {
  readRDS(population_file)
} else {
  res <- lapply(
    scenarios,
    function(x) population_analysis(x$scenario)
  )
  saveRDS(res, file = population_file)
  res
}
