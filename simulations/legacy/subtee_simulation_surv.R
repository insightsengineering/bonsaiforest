## Load data
load("scenarios/data_scenario1.RData")
load("scenarios/data_scenario2.RData")
load("scenarios/data_scenario3.RData")
load("scenarios/data_scenario4.RData")
load("scenarios/data_scenario5.RData")
load("scenarios/data_scenario6.RData")

scenario1 <- data_scenario1$scenario1
scenario2 <- data_scenario2$scenario2
scenario3 <- data_scenario3$scenario3
scenario4 <- data_scenario4$scenario4
scenario5 <- data_scenario5$scenario5
scenario6 <- data_scenario6$scenario6

#--------------------------------------------------------------------------------------------

subtee <- function(data) {
  library(subtee)
  covariates <- data[, 2:12]
  subgroup_model <- ~ x_1 + x_2 + x_3 + x_4 + x_5 + x_6 + x_7 + x_8 + x_9 + x_10
  design_main <- model.matrix(update(subgroup_model, ~ arm + .), data = covariates)[, -1]
  subgroup_vars <- all.vars(subgroup_model)

  # Design matrix and dummy encoding
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

  # Time and events
  y <- data[, 13:14]
  data_frame <- cbind(arm = data$arm, y, design_dummy, data)
  subgr <- colnames(design_dummy)

  res <- modav(
    resp = "tt_pfs", trt = "arm", subgr = subgr,
    covars = ~ x_1a + x_1b + x_2a + x_2b + x_3a + x_3b + x_4a + x_4b + x_4c +
      x_5a + x_5b + x_5c + x_5d + x_6a + x_6b + x_7a + x_7b + x_8a + x_8b + x_8c +
      x_9a + x_9b + x_10a + x_10b + x_10c, data = data_frame,
    fitfunc = "coxph", event = "ev_pfs", level = 0.05
  )
  HR <- exp(res$trtEff$trtEff)[seq(1, 50, 2)]
  l_HR <- exp(res$trtEff$LB)[seq(1, 50, 2)]
  u_HR <- exp(res$trtEff$UB)[seq(1, 50, 2)]
  AHR <- cbind(HR, l_HR, u_HR)
  return(AHR)
}


### -----------------------------------------------------------------------------------------

### Subtee simulation

# number of simulated datasets
n_datasets <- 1000


## Subtee HR: scenario1
set.seed(0)

subtee_HR1 <- matrix(NA, nrow = 25, ncol = n_datasets)
low_subtee1 <- matrix(NA, nrow = 25, ncol = n_datasets)
up_subtee1 <- matrix(NA, nrow = 25, ncol = n_datasets)

for (i in 1:n_datasets) {
  result <- subtee(scenario1[[i]])
  subtee_HR1[, i] <- result[, 1]
  low_subtee1[, i] <- result[, 2]
  up_subtee1[, i] <- result[, 3]
}
save(subtee_HR1, file = "Subtee/subtee_HR1.RData")
save(low_subtee1, file = "Subtee/low_subtee1.RData")
save(up_subtee1, file = "Subtee/up_subtee1.RData")


## Subtee HR: scenario2
set.seed(0)

subtee_HR2 <- matrix(NA, nrow = 25, ncol = n_datasets)
low_subtee2 <- matrix(NA, nrow = 25, ncol = n_datasets)
up_subtee2 <- matrix(NA, nrow = 25, ncol = n_datasets)

for (i in 1:n_datasets) {
  result <- subtee(scenario2[[i]])
  subtee_HR2[, i] <- result[, 1]
  low_subtee2[, i] <- result[, 2]
  up_subtee2[, i] <- result[, 3]
}
save(subtee_HR2, file = "Subtee/subtee_HR2.RData")
save(low_subtee2, file = "Subtee/low_subtee2.RData")
save(up_subtee2, file = "Subtee/up_subtee2.RData")



## Subtee HR: scenario3
set.seed(0)

subtee_HR3 <- matrix(NA, nrow = 25, ncol = n_datasets)
low_subtee3 <- matrix(NA, nrow = 25, ncol = n_datasets)
up_subtee3 <- matrix(NA, nrow = 25, ncol = n_datasets)

for (i in 1:n_datasets) {
  result <- subtee(scenario3[[i]])
  subtee_HR3[, i] <- result[, 1]
  low_subtee3[, i] <- result[, 2]
  up_subtee3[, i] <- result[, 3]
}
save(subtee_HR3, file = "Subtee/subtee_HR3.RData")
save(low_subtee3, file = "Subtee/low_subtee3.RData")
save(up_subtee3, file = "Subtee/up_subtee3.RData")


## Subtee HR: scenario4
set.seed(0)

subtee_HR4 <- matrix(NA, nrow = 25, ncol = n_datasets)
low_subtee4 <- matrix(NA, nrow = 25, ncol = n_datasets)
up_subtee4 <- matrix(NA, nrow = 25, ncol = n_datasets)

for (i in 1:n_datasets) {
  result <- subtee(scenario4[[i]])
  subtee_HR4[, i] <- result[, 1]
  low_subtee4[, i] <- result[, 2]
  up_subtee4[, i] <- result[, 3]
}
save(subtee_HR4, file = "Subtee/subtee_HR4.RData")
save(low_subtee4, file = "Subtee/low_subtee4.RData")
save(up_subtee4, file = "Subtee/up_subtee4.RData")



## Subtee HR: scenario5
set.seed(0)

subtee_HR5 <- matrix(NA, nrow = 25, ncol = n_datasets)
low_subtee5 <- matrix(NA, nrow = 25, ncol = n_datasets)
up_subtee5 <- matrix(NA, nrow = 25, ncol = n_datasets)

for (i in 1:n_datasets) {
  result <- subtee(scenario5[[i]])
  subtee_HR5[, i] <- result[, 1]
  low_subtee5[, i] <- result[, 2]
  up_subtee5[, i] <- result[, 3]
}

save(subtee_HR5, file = "Subtee/subtee_HR5.RData")
save(low_subtee5, file = "Subtee/low_subtee5.RData")
save(up_subtee5, file = "Subtee/up_subtee5.RData")



## Subtee HR: scenario6
set.seed(0)

subtee_HR6 <- matrix(NA, nrow = 25, ncol = n_datasets)
low_subtee6 <- matrix(NA, nrow = 25, ncol = n_datasets)
up_subtee6 <- matrix(NA, nrow = 25, ncol = n_datasets)

for (i in 1:n_datasets) {
  result <- subtee(scenario6[[i]])
  subtee_HR6[, i] <- result[, 1]
  low_subtee6[, i] <- result[, 2]
  up_subtee6[, i] <- result[, 3]
}
save(subtee_HR6, file = "Subtee/subtee_HR6.RData")
save(low_subtee6, file = "Subtee/low_subtee6.RData")
save(up_subtee6, file = "Subtee/up_subtee6.RData")
