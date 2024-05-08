# Calculate true (average) hazard ratios for the six scenarios considered.

# Setup ----

# Increase in sample size of simulated data for obtaining simulation truth.
inflation_factor <- 1000

# Number of simulation repetitions per scenario for obtaining simulation truth.
n_repetitions <- 10

# All subgroups.
subgroup_names <- c(
  "S_1a", "S_1b", "S_2a", "S_2b", "S_3a", "S_3b", "S_4a", "S_4b",
  "S_4c", "S_5a", "S_5b", "S_5c", "S_5d", "S_6a", "S_6b", "S_7a",
  "S_7b", "S_8a", "S_8b", "S_8c", "S_9a", "S_9b", "S_10a", "S_10b",
  "S_10c"
)

# All scenarios.
all_scenarios <- as.character(1:6)
scenario_names <- paste0("scenario", all_scenarios)

# All outcomes.
outcome_names <- c("HR", "AHR", "median_C", "median_I")

# Initialize data frame for true overall HR & AHR & median PFS in both groups.
true_overall_results <- init_data_frame(scenario_names, outcome_names)

# Initialize data frame for
# true subgroup HR (using coxph) and the same for
# AHR (estimated via KM curves in both groups).
true_subgroup_hr <- init_data_frame(scenario_names, subgroup_names)
true_subgroup_ahr <- true_subgroup_hr

# True HR and AHR for the "interaction" of subgrouping variables 1 and 2
# for the mis-specified scenario 6.
scen_6_subgroups <- c("x_1_2.aa", "x_1_2.ab", "x_1_2.ba", "x_1_2.bb")
scen_6_scenario <- scenario_names[6]
true_ia_subgroup_hr <- init_data_frame(scen_6_scenario, scen_6_subgroups)
true_ia_subgroup_ahr <- true_ia_subgroup_hr

# Calculation ----

tictoc::tic()

set.seed(23)

for (scenario in 1:6){
  all_overall_results <- matrix(nrow=4, ncol=nreps)

  all_subgroups_log_hr <- matrix(NA,nrow=length(subgroup_names),ncol=nreps)
  all_subgroups_log_ahr <- matrix(NA,nrow=length(subgroup_names),ncol=nreps)

  if (scenario==6){
    ia_subgroups_log_hr <- matrix(NA, nrow=4, ncol=nreps)
    ia_subgroups_log_ahr <- matrix(NA, nrow=4, ncol=nreps)
  }

  for (j in 1:nreps){ # simulation repetitions per scenario

    # simulate data
    sim_data <- simul_tte_scenarios(scenario = scenario, inflation_factor = inflation_factor, add_uncensored_pfs = T)

    # calculate results in overall population
    all_overall_results[1,j] <- coxph(Surv(tt_pfs,ev_pfs)~arm, data=sim_data)$coef
    t_max <- quantile(sim_data$tt_pfs[sim_data$ev_pfs==1],0.95) # evaluate "AHR integral" only up to 95% quantile of event times to avoid instability of KM estimates in tails
    all_overall_results[2,j] <- log(ahr(sim_data, resp="tt_pfs", status = "ev_pfs", t_max = t_max))

    all_overall_results[3,j] <- median(sim_data$tt_pfs_uncens[sim_data$arm==0])
    all_overall_results[4,j] <- median(sim_data$tt_pfs_uncens[sim_data$arm==1])

    # calculate HR and AHR in subgroups
    stacked_data <- generate_stacked_data(base_model,subgroup_model,sim_data,rename=T)

    naive_estimates <- stacked_data %>% group_by(subgroup) %>% do(tidy(survival::coxph(Surv(time,status)~arm,data=.)))
    all_subgroups_log_hr[,j] <- naive_estimates$estimate

    for (k in (1:length(subgroup_names))){
      data_subset_k <- subset(stacked_data,subgroup==subgroup_names[k])
      # Calculate AHR in subset k (evaluate "AHR integral" only up to 95% quantile of event times to avoid instability of KM estimates in tails)
      t_max <- quantile(data_subset_k$time[data_subset_k$status==1],0.95)
      all_subgroups_log_ahr[k,j] <- log(ahr(data_subset_k, t_max=t_max))
    }

    # calculate HR and AHR in intreraction subgroups for scenario 6
    if (scenario==6){
      sim_data$x_1_2 <- factor(with(sim_data,paste(as.character(x_1),as.character(x_2),sep="")))
      stacked_data_ia <- generate_stacked_data(base_model,~x_1_2,sim_data,rename=T)

      naive_estimates_ia <- stacked_data_ia %>% group_by(subgroup) %>% do(tidy(survival::coxph(Surv(time,status)~arm,data=.)))
      ia_subgroups_log_hr[,j] <- naive_estimates_ia$estimate

      subgroup_names_ia <- c("x_1_2.aa", "x_1_2.ab", "x_1_2.ba", "x_1_2.bb")

      for (k in (1:length(subgroup_names_ia))){
        data_subset_k <- subset(stacked_data_ia,subgroup==subgroup_names_ia[k])
        # Calculate AHR in subset k (evaluate "AHR integral" only up to 95% quantile of event times to avoid instability of KM estimates in tails)
        t_max <- quantile(data_subset_k$time[data_subset_k$status==1],0.95)
        ia_subgroups_log_ahr[k,j] <- log(ahr(data_subset_k, t_max=t_max))
      }
    }
  }

  # aggregate across repeated runs
  true_overall_results[scenario,"HR"] <- exp(mean(all_overall_results[1,]))
  true_overall_results[scenario,"AHR"] <- exp(mean(all_overall_results[2,]))
  true_overall_results[scenario,"median_C"] <- mean(all_overall_results[3,])
  true_overall_results[scenario,"median_I"] <- mean(all_overall_results[4,])

  true_subgroup_hr[scenario,] <- exp(apply(all_subgroups_log_hr,1,mean))
  true_subgroup_ahr[scenario,] <- exp(apply(all_subgroups_log_ahr,1,mean))

  if (scenario==6){
    true_ia_subgroup_hr[1,] <- exp(apply(ia_subgroups_log_hr,1,mean))
    true_ia_subgroup_ahr[1,] <- exp(apply(ia_subgroups_log_ahr,1,mean))
  }

}

tictoc::toc()

#-- Save results
simul_parameter <- list(true_overall_results = true_overall_results,
                        true_subgroup_hr = true_subgroup_hr,
                        true_subgroup_ahr = true_subgroup_ahr,
                        true_ia_subgroup_hr = true_ia_subgroup_hr,
                        true_ia_subgroup_ahr = true_ia_subgroup_ahr)
save(simul_parameter, file = "scenarios/truth.Rdata")
