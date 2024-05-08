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

# Simulate data sets from a given scenario, defined by named vector of coefficients in
# `coefs`.
simul_scenario <- function(scenario = c("1", "2", "3", "4", "5", "6"),
                           n_datasets = 1000,
                           n_patients = 1000,
                           n_events = 247,
                           sigma_aft = 0.85,
                           # Recruitment duration in years:
                           recr_duration = 3,
                           # Censoring rate at 1 year:
                           rate_cens = 0.02) {
  scenario <- match.arg(scenario)
  assert_count(n_datasets)

  # Set constant (across scenarios) intercept and prognostic factors.
  constant_coefs <- c(
    "(Intercept)" = 2,
    "x_4c" = - log(0.7) * sigma_aft,
    "x_6b" = - log(1.5) * sigma_aft
  )

  # Names of all the subgroup specific coefficients.
  group_coefs_names <- c(
    "x_1a_arm", "x_1b_arm", "x_2a_arm", "x_2b_arm",
    "x_3a_arm", "x_3b_arm", "x_4a_arm", "x_4b_arm", "x_4c_arm", "x_5a_arm",
    "x_5b_arm", "x_5c_arm", "x_5d_arm", "x_6a_arm", "x_6b_arm", "x_7a_arm",
    "x_7b_arm", "x_8a_arm", "x_8b_arm", "x_8c_arm", "x_9a_arm", "x_9b_arm",
    "x_10a_arm", "x_10b_arm", "x_10c_arm"
  )

  coefs <- switch(
    scenario,
    # Positive trial, homogeneous treatment effect.
    "1" = c(
      constant_coefs,
      "arm" = - log(0.66) * sigma_aft
    ),
    # Overall HR~0.66, but no effect in x_4a.
    "2" = c(
      constant_coefs,
      "arm" = - log(0.66) * sigma_aft,
      # No effect in x_4a:
      "x_4a_arm" = log(0.66) * sigma_aft,
      # Slightly enhanced effect in x_4b and x_4c to "compensate" no effect in x_4a:
      "x_4b_arm" = - log(0.8) * sigma_aft,
      "x_4c_arm" = - log(0.8) * sigma_aft
    ),
    # Overall HR~1, but HR~0.5 in x_4a.
    "3" = c(
      constant_coefs,
      "arm" = 0,
      "x_4a_arm" = - log(0.5) * sigma_aft,
      # Detrimental effect in x_4b and x_4c to "compensate" effect in x_4a:
      "x_4b_arm" = - log(1.25) * sigma_aft,
      "x_4c_arm" = - log(1.25) * sigma_aft
    ),
    # Mild heterogeneity.
    "4" = {
      set.seed(5)
      c(
        constant_coefs,
        "arm" = 0,
        setNames(
          - rnorm(25, sd = 0.15) * sigma_aft,
          group_coefs_names
        )
      )
    },
    # Large heterogeneity.
    "5" = {
      set.seed(5)
      c(
        constant_coefs,
        arm = 0,
        setNames(
          - rnorm(25, sd = 0.3) * sigma_aft,
          group_coefs_names
        )
      )
    },
    # Model with interaction.
    "6" = c(
      constant_coefs,
      "arm" = - log(0.66) * sigma_aft,
      "x_1_2aa_arm" = - log(1.5) * sigma_aft,
      "x_1_2ba_arm" = - log(0.5) * sigma_aft,
      "x_1_2ab_arm" = - log(0.92) * sigma_aft,
      "x_1_2bb_arm" = - log(1.07) * sigma_aft
    )
  )
  add_interaction <- scenario == "6"

  replicate(
    n_datasets,
    simul_data(
      n = n_patients,
      coefs = coefs,
      add_interaction = add_interaction,
      sigma_aft = sigma_aft,
      recr_duration = recr_duration,
      rate_cens = rate_cens,
      n_events = n_events
    ),
    simplify = FALSE
  )
}

init_data_frame <- function(row_names, col_names) {
  assert_character(row_names)
  assert_character(col_names)
  data.frame(matrix(
    nrow = length(row_names),
    ncol = length(col_names),
    dimnames = list(
      row_names,
      col_names
    )
  ))
}
