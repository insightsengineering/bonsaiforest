# Simulate data from the six different scenarios used.

library(tidyverse)
library(survival)
library(forestplot)
library(bonsaiforest)

## Simulation of scenarios

#Set seed
set.seed(0)

# sigma_aft (same for all scenarios)
sigma_aft <- 0.85

# Dummy vector for AFT regression coefficient (with over-parametrized dummy coding for arm-covariate interactions)
coef_raw <- rep(0,42)
names(coef_raw) <- c("(Intercept)","arm","x_1b","x_2b","x_3b","x_4b","x_4c",
                     "x_5b","x_5c","x_5d","x_6b","x_7b","x_8b","x_8c","x_9b","x_10b","x_10c",
                     "x_1a_arm","x_1b_arm","x_2a_arm","x_2b_arm","x_3a_arm","x_3b_arm","x_4a_arm","x_4b_arm","x_4c_arm",
                     "x_5a_arm","x_5b_arm","x_5c_arm","x_5d_arm","x_6a_arm","x_6b_arm","x_7a_arm","x_7b_arm","x_8a_arm",
                     "x_8b_arm","x_8c_arm","x_9a_arm","x_9b_arm","x_10a_arm","x_10b_arm","x_10c_arm")


# Set intercept and prognostic factors (same for all scenarios)
coef_raw["(Intercept)"] <- 2 # use 2 rather than old version 4.5 to change scale to years rather than months
coef_raw["x_4c"] <- -log(.7)*sigma_aft
coef_raw["x_6b"] <- -log(1.5)*sigma_aft


# number of simulated datasets
n_datasets <- 1000



#-- Scenario 1 - positive trial, homogeneous treatment effect
coef_1 <- coef_raw
coef_1["arm"] <- -log(0.66)*sigma_aft


scenario1 <- list()

for (i in 1:n_datasets){
  d <- simul_data(n=1000,coef=coef_1,sigma_aft=sigma_aft,recr_duration=3,rate_cens=0.02,n_events=247)
  scenario1 <- append(scenario1, list(d))
}

save(scenario1, file="Scenarios/scenario1.RData")



#-- Scenario 2: overall HR~0.66, but no effect in x_4a
coef_2 <- coef_raw
coef_2["arm"] <- -log(0.66)*sigma_aft
coef_2["x_4a_arm"] <- -coef_2["arm"]         # no effect in x_4a

coef_2["x_4b_arm"] <- -log(0.8)*sigma_aft    # slightly enhanced efffect in x_4b and x_4c to "compensate" no effect in x_4a
coef_2["x_4c_arm"] <- -log(0.8)*sigma_aft    # slightly enhanced efffect in x_4b and x_4c to "compensate" no effect in x_4a


scenario2 <- list()

for (i in 1:n_datasets){
  d <- simul_data(n=1000,coef=coef_2,sigma_aft=sigma_aft,recr_duration=3,rate_cens=0.02,n_events=247)
  scenario2 <- append(scenario2, list(d))
}

save(scenario2, file="Scenarios/scenario2.RData")



#-- Scenario 3: overall HR~1, but HR~0.5 in x_4a
coef_3 <- coef_raw
coef_3["arm"] <- 0
coef_3["x_4a_arm"] <- -log(0.5)*sigma_aft
coef_3["x_4b_arm"] <- -log(1.25)*sigma_aft     # detrimental effect in x_4b and x_4c to "compensate" effect in x_4a
coef_3["x_4c_arm"] <- -log(1.25)*sigma_aft     # detrimental effect in x_4b and x_4c to "compensate" effect in x_4a

scenario3 <- list()

for (i in 1:n_datasets){
  d <- simul_data(n=1000,coef=coef_3,sigma_aft=sigma_aft,recr_duration=3,rate_cens=0.02,n_events=247)
  scenario3 <- append(scenario3, list(d))
}

save(scenario3, file="Scenarios/scenario3.RData")



#-- Scenario 4: mild heterogeneity
coef_4 <- coef_raw
coef_4["arm"] <- 0
set.seed(5)
log_hr_tmp <- rnorm(25,sd=0.15)
coef_4[18:42] <- -log_hr_tmp*sigma_aft

scenario4 <- list()

for (i in 1:n_datasets){
  d <- simul_data(n=1000,coef=coef_4,sigma_aft=sigma_aft,recr_duration=3,rate_cens=0.02,n_events=247)
  scenario4 <- append(scenario4, list(d))
}

save(scenario4, file="Scenarios/scenario4.RData")



#-- Scenario 5: large heterogeneity
coef_5 <- coef_raw
coef_5["arm"] <- 0
set.seed(5)
log_hr_tmp <- rnorm(25,sd=0.3)
coef_5[18:42] <- -log_hr_tmp*sigma_aft

scenario5 <- list()

for (i in 1:n_datasets){
  d <- simul_data(n=1000,coef=coef_5,sigma_aft=sigma_aft,recr_duration=3,rate_cens=0.02,n_events=247)
  scenario5 <- append(scenario5, list(d))
}

save(scenario5, file="Scenarios/scenario5.RData")


#-- Scenario 6: model with interaction
coef_raw <- rep(0,47)
names(coef_raw) <- c("(Intercept)","arm","x_1b","x_2b", "x_3b","x_4b","x_4c",
                     "x_5b","x_5c","x_5d","x_6b","x_7b","x_8b","x_8c","x_9b","x_10b","x_10c", "x_1b:x_2b",
                     "x_1a_arm","x_1b_arm","x_2a_arm","x_2b_arm","x_3a_arm","x_3b_arm","x_4a_arm","x_4b_arm","x_4c_arm",
                     "x_5a_arm","x_5b_arm","x_5c_arm","x_5d_arm","x_6a_arm","x_6b_arm","x_7a_arm","x_7b_arm","x_8a_arm",
                     "x_8b_arm","x_8c_arm","x_9a_arm","x_9b_arm","x_10a_arm","x_10b_arm","x_10c_arm",
                     "x_1a2a_arm","x_1a2b_arm","x_1b2a_arm","x_1b2b_arm")

coef_raw["(Intercept)"] <- 2 # use 2 rather than old version 4.5 to change scale to years rather than months
coef_raw["x_4c"] <- -log(.7)*sigma_aft
coef_raw["x_6b"] <- -log(1.5)*sigma_aft
coef_6 <- coef_raw
coef_6["arm"] <- -log(0.66)*sigma_aft

coef_6["x_1a2a_arm"] <- -log(1.5)*sigma_aft
coef_6["x_1b2a_arm"] <- -log(0.5)*sigma_aft
coef_6["x_1a2b_arm"] <- -log(0.92)*sigma_aft
coef_6["x_1b2b_arm"] <- -log(1.07)*sigma_aft

scenario6 <- list()

for (i in 1:n_datasets){
  d <- create_data(n=1000,coef=coef_6,sigma_aft=sigma_aft,recr_duration=3,rate_cens=0.02,n_events=247)
  scenario6 <- append(scenario6, list(d))
}

save(scenario6, file="Scenarios/scenario6.RData")
