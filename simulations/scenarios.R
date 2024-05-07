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
recr_duration <- 3
rate_cens <- 0.02
n_events <- 247
out_dir <- "scenarios"

# Dummy vector for AFT regression coefficients
# (with over-parametrized dummy coding for arm-covariate interactions).
coef_raw <- rep(0, 42)
names(coef_raw) <- c(
  "(Intercept)", "arm", "x_1b", "x_2b", "x_3b", "x_4b", "x_4c",
  "x_5b", "x_5c", "x_5d", "x_6b", "x_7b", "x_8b", "x_8c", "x_9b", "x_10b", "x_10c",
  "x_1a_arm", "x_1b_arm", "x_2a_arm", "x_2b_arm", "x_3a_arm", "x_3b_arm", "x_4a_arm", "x_4b_arm", "x_4c_arm",
  "x_5a_arm", "x_5b_arm", "x_5c_arm", "x_5d_arm", "x_6a_arm", "x_6b_arm", "x_7a_arm", "x_7b_arm", "x_8a_arm",
  "x_8b_arm", "x_8c_arm", "x_9a_arm", "x_9b_arm", "x_10a_arm", "x_10b_arm", "x_10c_arm"
)

# Set intercept and prognostic factors.
coef_raw["(Intercept)"] <- 2
coef_raw["x_4c"] <- -log(.7) * sigma_aft
coef_raw["x_6b"] <- -log(1.5) * sigma_aft

# Helper function with which data from each scenario will be simulated.
sim_scenario <- function(coef, ...) {
  replicate(
    n_datasets,
    simul_data(
      n = n_patients,
      coef = coef,
      sigma_aft = sigma_aft,
      recr_duration = recr_duration,
      rate_cens = rate_cens,
      n_events = n_events,
      ...
    ),
    simplify = FALSE
  )
}

# Scenario 1 ----
# Positive trial, homogeneous treatment effect.

coef_1 <- coef_raw
coef_1["arm"] <- -log(0.66) * sigma_aft

scenario1 <- sim_scenario(coef = coef_1)
save(scenario1, file = file.path(out_dir, "scenario1.RData"))

# Scenario 2 ----
# Overall HR~0.66, but no effect in x_4a.

coef_2 <- coef_raw
coef_2["arm"] <- -log(0.66) * sigma_aft
# No effect in x_4a:
coef_2["x_4a_arm"] <- -coef_2["arm"]
# Slightly enhanced effect in x_4b and x_4c to "compensate" no effect in x_4a:
coef_2["x_4b_arm"] <- -log(0.8) * sigma_aft
# Slightly enhanced effect in x_4b and x_4c to "compensate" no effect in x_4a:
coef_2["x_4c_arm"] <- -log(0.8) * sigma_aft

scenario2 <- sim_scenario(coef = coef_2)
save(scenario2, file = file.path(out_dir, "scenario2.RData"))

# Scenario 3 ----
# Overall HR~1, but HR~0.5 in x_4a.

coef_3 <- coef_raw
coef_3["arm"] <- 0
coef_3["x_4a_arm"] <- -log(0.5) * sigma_aft
# Detrimental effect in x_4b and x_4c to "compensate" effect in x_4a:
coef_3["x_4b_arm"] <- -log(1.25) * sigma_aft
# Detrimental effect in x_4b and x_4c to "compensate" effect in x_4a:
coef_3["x_4c_arm"] <- -log(1.25) * sigma_aft

scenario3 <- sim_scenario(coef = coef_3)
save(scenario3, file = file.path(out_dir, "scenario3.RData"))

# Scenario 4 ----
# Mild heterogeneity.

coef_4 <- coef_raw
coef_4["arm"] <- 0
set.seed(5)
log_hr_tmp <- rnorm(25, sd = 0.15)
coef_4[18:42] <- -log_hr_tmp * sigma_aft

scenario4 <- sim_scenario(coef = coef_4)
save(scenario4, file = file.path(out_dir, "scenario4.RData"))

# Scenario 5 ----
# Large heterogeneity.

coef_5 <- coef_raw
coef_5["arm"] <- 0
set.seed(5)
log_hr_tmp <- rnorm(25, sd = 0.3)
coef_5[18:42] <- -log_hr_tmp * sigma_aft

scenario5 <- sim_scenario(coef = coef_5)
save(scenario5, file = file.path(out_dir, "scenario5.RData"))

# Scenario 6 ----
# Model with interaction.

coef_raw <- rep(0, 47)
names(coef_raw) <- c(
  "(Intercept)", "arm", "x_1b", "x_2b", "x_3b", "x_4b", "x_4c",
  "x_5b", "x_5c", "x_5d", "x_6b", "x_7b", "x_8b", "x_8c", "x_9b", "x_10b", "x_10c", "x_1b:x_2b",
  "x_1a_arm", "x_1b_arm", "x_2a_arm", "x_2b_arm", "x_3a_arm", "x_3b_arm", "x_4a_arm", "x_4b_arm", "x_4c_arm",
  "x_5a_arm", "x_5b_arm", "x_5c_arm", "x_5d_arm", "x_6a_arm", "x_6b_arm", "x_7a_arm", "x_7b_arm", "x_8a_arm",
  "x_8b_arm", "x_8c_arm", "x_9a_arm", "x_9b_arm", "x_10a_arm", "x_10b_arm", "x_10c_arm",
  "x_1a2a_arm", "x_1a2b_arm", "x_1b2a_arm", "x_1b2b_arm"
)

coef_raw["(Intercept)"] <- 2
coef_raw["x_4c"] <- -log(.7) * sigma_aft
coef_raw["x_6b"] <- -log(1.5) * sigma_aft
coef_6 <- coef_raw
coef_6["arm"] <- -log(0.66) * sigma_aft

coef_6["x_1a2a_arm"] <- -log(1.5) * sigma_aft
coef_6["x_1b2a_arm"] <- -log(0.5) * sigma_aft
coef_6["x_1a2b_arm"] <- -log(0.92) * sigma_aft
coef_6["x_1b2b_arm"] <- -log(1.07) * sigma_aft

scenario6 <- sim_scenario(coef = coef_6, add_interaction = TRUE)
save(scenario6, file = file.path(out_dir, "scenario6.RData"))
