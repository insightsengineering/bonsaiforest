# Create example data.
library(usethis)
library(MASS)

# Set seed
set.seed(0)

# sigma_aft
sigma_aft <- 0.85

# Dummy vector for AFT regression coefficient (with over-parametrized dummy
# coding for arm-covariate interactions)
coef_raw <- rep(0, 42)
names(coef_raw) <- c(
  "(Intercept)", "arm", "x_1b", "x_2b", "x_3b", "x_4b", "x_4c",
  "x_5b", "x_5c", "x_5d", "x_6b", "x_7b", "x_8b", "x_8c",
  "x_9b", "x_10b", "x_10c", "x_1a_arm", "x_1b_arm", "x_2a_arm",
  "x_2b_arm", "x_3a_arm", "x_3b_arm", "x_4a_arm", "x_4b_arm",
  "x_4c_arm", "x_5a_arm", "x_5b_arm", "x_5c_arm", "x_5d_arm",
  "x_6a_arm", "x_6b_arm", "x_7a_arm", "x_7b_arm", "x_8a_arm",
  "x_8b_arm", "x_8c_arm", "x_9a_arm", "x_9b_arm", "x_10a_arm",
  "x_10b_arm", "x_10c_arm"
)

# Set intercept and prognostic factors (same for all scenarios)
coef_raw["(Intercept)"] <- 2 # use 2 rather than old version 4.5 to change scale to years rather than months
coef_raw["x_4c"] <- -log(0.7) * sigma_aft
coef_raw["x_6b"] <- -log(1.5) * sigma_aft



#-- Example-data (positive trial, homogeneous treatment effect)
coef_1 <- coef_raw
coef_1["arm"] <- -log(0.66) * sigma_aft

example_data <- simul_data(
  n = 1000, coef = coef_1, sigma_aft = sigma_aft,
  recr_duration = 3, rate_cens = 0.02, n_events = 247
)
example_data$arm <- as.factor(example_data$arm)
usethis::use_data(example_data)
