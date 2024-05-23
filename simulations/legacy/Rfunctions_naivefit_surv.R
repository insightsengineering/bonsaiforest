#--------------------- functions to generative naive subgoup-specific treatment effect estimates (survival data)

generate_stacked_data <- function(base_model, subgroup_model, data, id_var = "id", rename = T) {
  # base_model:       must be of the form Surv(time,status)~arm
  # subgroup_model:   must be of the form ~x1+x2 with x1 and x2 factors or character variables
  # data:             data frame including all variables
  # id_var:           name of variable containing unique patient identifier
  # rename:           if F, keep original variable names, if T, change id_var, time-to-event, and arm variables to "default names" needed for other functions
  require(tidyverse)
  data <- as_tibble(data)
  # Preparation of models and data
  subgroup_vars <- all.vars(subgroup_model)
  time_var <- all.vars(base_model)[1]
  status_var <- all.vars(base_model)[2]
  arm_var <- all.vars(base_model)[3]
  # Subgroup names in "correct" order
  tmp <- lapply(data[, subgroup_vars], function(x) levels(factor(x)))
  subgroup_names <- paste(rep(names(tmp), lapply(tmp, length)), unlist(tmp), sep = ".")
  # create long dataset
  data <- data[, c(id_var, arm_var, time_var, status_var, subgroup_vars)]
  data[, subgroup_vars] <- lapply(data[, subgroup_vars], as.character)
  d <- gather(data, "subgroup_var", "subgroup_value", -id_var, -arm_var, -time_var, -status_var)
  d$subgroup <- paste(d$subgroup_var, d$subgroup_value, sep = ".")
  d$subgroup <- factor(d$subgroup, levels = subgroup_names)
  d <- arrange(d, subgroup) # sort by "correct" variable order
  if (rename) d <- rename_at(d, vars(c(id_var, arm_var, time_var, status_var)), ~ c("id", "arm", "time", "status"))
  d
}


# Function to get naive subgroup estimates in subgroups
# In the end, this function was not used as coxphw is very slow and it was easier to get an alternative AHR function based on the KM estimator

tidy.coxphw <- function(x, ...) {
  ret <- as_tibble(cbind("estimate" = x$coefficients, "std.error" = sqrt(diag(x$var))))
  ret
}

calc_naive_subgroup_estimates <- function(base_model, subgroup_model, data, coxphw = FALSE) {
  require(survival)
  require(coxphw)
  require(tidyverse)
  require(broom)
  stacked_data <- generate_stacked_data(base_model, subgroup_model, data, rename = T)
  # PS: alternative "trick" to get naive subgroup-specific estimates using coxph: coxph(Surv(time,status)~arm*strata(subgroup)-arm,data=stacked_data)
  if (coxphw == FALSE) {
    naive_estimates <- stacked_data %>%
      group_by(subgroup) %>%
      do(tidy(survival::coxph(Surv(time, status) ~ arm, data = .)))
  }
  if (coxphw == TRUE) {
    naive_estimates <- stacked_data %>%
      group_by(subgroup) %>%
      do(tidy(coxphw::coxphw(Surv(time, status) ~ arm, template = "AHR", data = .)))
  }
  naive_estimates$exp.estimate <- exp(naive_estimates$estimate)
  naive_estimates$exp.conf.low <- exp(naive_estimates$estimate - qnorm(0.975) * naive_estimates$std.error)
  naive_estimates$exp.conf.high <- exp(naive_estimates$estimate + qnorm(0.975) * naive_estimates$std.error)
  naive_estimates[, c("subgroup", "estimate", "std.error", "exp.estimate", "exp.conf.low", "exp.conf.high")]
}

# Function to get the average hazard ratio based on KM-estimates in both group
# Note: This function is new! Estimates can be unstable due to the variability of the KM-estimates in the tail
ahr <- function(d, resp = "time", status = "status", trt = "arm", t_max = Inf) {
  # Simple function to estimate the AHR based on Kaplan-Meier estimation
  # Argument t_max can be used to restrict the time range (to reduce impact of instable of KM estimates in the tails)
  t <- sort(unique(d[[resp]][d[[status]] == 1])) # unique event times
  t <- t[t < t_max]
  # KM at t in both groups
  base_model <- stats::as.formula(paste("Surv(", resp, ",", status, ") ~ ", trt))
  km_C <- summary(survfit(base_model, data = d, subset = (arm == 0)), times = t, extend = T)$surv
  km_I <- summary(survfit(base_model, data = d, subset = (arm == 1)), times = t, extend = T)$surv
  # AHR
  sum(km_C * diff(c(1, km_I))) / sum(km_I * diff(c(1, km_C)))
}
