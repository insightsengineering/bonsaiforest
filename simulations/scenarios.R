# Simulate data from the six different scenarios used.

library(tidyverse)
library(survival)
library(forestplot)
library(bonsaiforest)

# Common settings ----

set.seed(0)
n_datasets <- 1000
n_patients <- 1000
sigma_aft <- 0.85
# Recruitment duration in years:
recr_duration <- 3
# Censoring rate at 1 year:
rate_cens <- 0.02
n_events <- 247
out_dir <- "scenarios"

# Set intercept and prognostic factors.
coefs <- c(
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

# Helper function with which data from each scenario will be simulated.
sim_scenario <- function(this_coefs, this_interaction = FALSE) {
  replicate(
    n_datasets,
    simul_data(
      n = n_patients,
      coefs = this_coefs,
      add_interaction = this_interaction,
      sigma_aft = sigma_aft,
      recr_duration = recr_duration,
      rate_cens = rate_cens,
      n_events = n_events
    ),
    simplify = FALSE
  )
}

# Scenario 1 ----
# Positive trial, homogeneous treatment effect.

coefs1 <- c(coefs, "arm" = - log(0.66) * sigma_aft)
scenario1 <- sim_scenario(this_coefs = coefs1)
save(scenario1, file = file.path(out_dir, "scenario1.RData"))

# Scenario 2 ----
# Overall HR~0.66, but no effect in x_4a.

coefs2 <- c(
  coefs,
  "arm" = - log(0.66) * sigma_aft,
  # No effect in x_4a:
  "x_4a_arm" = log(0.66) * sigma_aft,
  # Slightly enhanced effect in x_4b and x_4c to "compensate" no effect in x_4a:
  "x_4b_arm" = - log(0.8) * sigma_aft,
  "x_4c_arm" = - log(0.8) * sigma_aft
)
scenario2 <- sim_scenario(this_coefs = coefs2)
save(scenario2, file = file.path(out_dir, "scenario2.RData"))

# Scenario 3 ----
# Overall HR~1, but HR~0.5 in x_4a.

coefs3 <- c(
  coefs,
  "arm" = 0,
  "x_4a_arm" = - log(0.5) * sigma_aft,
  # Detrimental effect in x_4b and x_4c to "compensate" effect in x_4a:
  "x_4b_arm" = - log(1.25) * sigma_aft,
  "x_4c_arm" = - log(1.25) * sigma_aft
)
scenario3 <- sim_scenario(this_coefs = coefs3)
save(scenario3, file = file.path(out_dir, "scenario3.RData"))

# Scenario 4 ----
# Mild heterogeneity.

set.seed(5)
coefs4 <- c(
  coefs,
  "arm" = 0,
  setNames(
    - rnorm(25, sd = 0.15) * sigma_aft,
    group_coefs_names
  )
)
scenario4 <- sim_scenario(this_coefs = coefs4)
save(scenario4, file = file.path(out_dir, "scenario4.RData"))

# Scenario 5 ----
# Large heterogeneity.

set.seed(5)
coefs5 <- c(
  coefs,
  arm = 0,
  setNames(
    - rnorm(25, sd = 0.3) * sigma_aft,
    group_coefs_names
  )
)
scenario5 <- sim_scenario(this_coefs = coefs5)
save(scenario5, file = file.path(out_dir, "scenario5.RData"))

# Scenario 6 ----
# Model with interaction.

coefs6 <- c(
  coefs,
  "arm" = - log(0.66) * sigma_aft,
  "x_1_2aa_arm" = - log(1.5) * sigma_aft,
  "x_1_2ba_arm" = - log(0.5) * sigma_aft,
  "x_1_2ab_arm" = - log(0.92) * sigma_aft,
  "x_1_2bb_arm" = - log(1.07) * sigma_aft
)
scenario6 <- sim_scenario(this_coefs = coefs6, this_interaction = TRUE)
save(scenario6, file = file.path(out_dir, "scenario6.RData"))
