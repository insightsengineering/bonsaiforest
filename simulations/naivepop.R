# Method for a single data set.
naivepop_method <- function(df) {
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
naivepop_analysis <- function(scenario) {
  assert_list(scenario)
  results <- t(sapply(scenario, naivepop_method))
}

# Results across all scenarios.
naivepop_file <- "results/naivepop.rds"
naivepop_results <- if (file.exists(naivepop_file)) {
  readRDS(naivepop_file)
} else {
  res <- lapply(
    scenarios,
    function(x) naivepop_analysis(x$scenario)
  )
  saveRDS(res, file = naivepop_file)
  res
}
