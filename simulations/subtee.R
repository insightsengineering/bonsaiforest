# Method for a single data set.
subtee_method <- function(df, simul_no) {
  assert_data_frame(df)
  assert_count(simul_no)
  
  covariates <- df[, 2:12]
  subgroup_model <- ~ x_1 + x_2 + x_3 + x_4 + x_5 + x_6 + x_7 + x_8 + x_9 + x_10
  design_main <- model.matrix(update(subgroup_model, ~ arm + .), data = covariates)[, -1]
  subgroup_vars <- all.vars(subgroup_model)

  # Set up the design matrix and dummy encoding.
  design_ia <- NULL
  design_dummy <- NULL
  for (j in subgroup_vars) {
    ia_j <- model.matrix(as.formula(paste("~", j, "-1")), data = covariates) * covariates$arm
    design_ia <- cbind(design_ia, ia_j)
    ib_j <- model.matrix(as.formula(paste("~", j, "-1")), data = covariates)
    design_dummy <- cbind(design_dummy, ib_j)
  }

  colnames(design_ia) <- paste(colnames(design_ia), "arm", sep = "_")
  colnames(design_ia) <- gsub(" ", "", colnames(design_ia)) # remove any spaces
  design_matrix <- cbind(design_main, design_ia)

  # Use model averaging for treatment effect estimation.
  y <- df[, 13:14]
  data_frame <- cbind(arm = df$arm, y, design_dummy, df)
  subgr <- colnames(design_dummy)

  res <- modav(
    resp = "tt_pfs", trt = "arm", subgr = subgr,
    covars = ~ x_1a + x_1b + x_2a + x_2b + x_3a + x_3b + x_4a + x_4b + x_4c +
      x_5a + x_5b + x_5c + x_5d + x_6a + x_6b + x_7a + x_7b + x_8a + x_8b + x_8c +
      x_9a + x_9b + x_10a + x_10b + x_10c, data = data_frame,
    fitfunc = "coxph", event = "ev_pfs", level = 0.05
  )

  # Extract subgroup estimates.
  res_subgroups <- subset(res$trtEff, Subset == "Subgroup")
  with(
    res_subgroups,
    data.frame(
      simul_no = simul_no,
      estimator = "subtee",
      subgroup = sanitize_subgroups(as.character(Group)),
      estimate_ahr = exp(trtEff),
      estimate_log_ahr = trtEff,
      lower_ci_ahr = exp(LB),
      upper_ci_ahr = exp(UB)
    )
  )
}

# Analysis of a single scenario.
subtee_analysis <- fun_analysis(subtee_method)

# Results across all scenarios.
subtee_results <- compute_results(
  scenarios,
  analyze = subtee_analysis,
  cache = "results/subtee.rds"
)
