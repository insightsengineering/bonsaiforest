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

source("Rfunctions_naivefit_surv.R")

###-----------------------------------------------------------------------------------------------------------------------------

### Simulations:

library(tidyverse)
library(survival)
library(forestplot)

# models for subgroup estimation
base_model <- Surv(tt_pfs,ev_pfs)~arm
subgroup_model <- ~x_1+x_2+x_3+x_4+x_5+x_6+x_7+x_8+x_9+x_10

# number of simulated datasets
n_datasets <- 1000


## Naive HR: scenario1

naive_HR1 <- matrix(NA,nrow=25,ncol=n_datasets)
low_HR1 <- matrix(NA,nrow=25,ncol=n_datasets)
high_HR1 <- matrix(NA,nrow=25,ncol=n_datasets)

for (i in 1:n_datasets){
  result <- calc_naive_subgroup_estimates(base_model,subgroup_model,data=scenario1[[i]])
  naive_HR1[,i] <- result$exp.estimate
  low_HR1[, i] <- result$exp.conf.low
  high_HR1[, i] <- result$exp.conf.high
}

save(naive_HR1,file="Naive/naive_HR1.RData")
save(low_HR1, file="Naive/Conf/low_HR1.RData")
save(high_HR1, file="Naive/Conf/high_HR1.RData")


## Naive HR: scenario2

naive_HR2 <- matrix(NA,nrow=25,ncol=n_datasets)
low_HR2 <- matrix(NA,nrow=25,ncol=n_datasets)
high_HR2 <- matrix(NA,nrow=25,ncol=n_datasets)


for (i in 1:n_datasets){
  result <- calc_naive_subgroup_estimates(base_model,subgroup_model,data=scenario2[[i]])
  naive_HR2[,i] <- result$exp.estimate
  low_HR2[, i] <- result$exp.conf.low
  high_HR2[, i] <- result$exp.conf.high
}

save(naive_HR2,file="Naive/naive_HR2.RData")
save(low_HR2, file="Naive/Conf/low_HR2.RData")
save(high_HR2, file="Naive/Conf/high_HR2.RData")


## Naive HR: scenario3

naive_HR3 <- matrix(NA,nrow=25,ncol=n_datasets)
low_HR3 <- matrix(NA,nrow=25,ncol=n_datasets)
high_HR3 <- matrix(NA,nrow=25,ncol=n_datasets)


for (i in 1:n_datasets){
  result <- calc_naive_subgroup_estimates(base_model,subgroup_model,data=scenario3[[i]])
  naive_HR3[,i] <- result$exp.estimate
  low_HR3[, i] <- result$exp.conf.low
  high_HR3[, i] <- result$exp.conf.high
}

save(naive_HR3,file="Naive/naive_HR3.RData")
save(low_HR3, file="Naive/Conf/low_HR3.RData")
save(high_HR3, file="Naive/Conf/high_HR3.RData")


## Naive HR: scenario4

naive_HR4 <- matrix(NA,nrow=25,ncol=n_datasets)
low_HR4 <- matrix(NA,nrow=25,ncol=n_datasets)
high_HR4 <- matrix(NA,nrow=25,ncol=n_datasets)


for (i in 1:n_datasets){
  result <- calc_naive_subgroup_estimates(base_model,subgroup_model,data=scenario4[[i]])
  naive_HR4[,i] <- result$exp.estimate
  low_HR4[, i] <- result$exp.conf.low
  high_HR4[, i] <- result$exp.conf.high
}

save(naive_HR4,file="Naive/naive_HR4.RData")
save(low_HR4, file="Naive/Conf/low_HR4.RData")
save(high_HR4, file="Naive/Conf/high_HR4.RData")


## Naive HR: scenario5

naive_HR5 <- matrix(NA,nrow=25,ncol=n_datasets)
low_HR5 <- matrix(NA,nrow=25,ncol=n_datasets)
high_HR5 <- matrix(NA,nrow=25,ncol=n_datasets)


for (i in 1:n_datasets){
  result <- calc_naive_subgroup_estimates(base_model,subgroup_model,data=scenario5[[i]])
  naive_HR5[,i] <- result$exp.estimate
  low_HR5[, i] <- result$exp.conf.low
  high_HR5[, i] <- result$exp.conf.high
}

save(naive_HR5,file="Naive/naive_HR5.RData")
save(low_HR5, file="Naive/Conf/low_HR5.RData")
save(high_HR5, file="Naive/Conf/high_HR5.RData")

## Naive HR: scenario6

naive_HR6 <- matrix(NA,nrow=25,ncol=n_datasets)
low_HR6 <- matrix(NA,nrow=25,ncol=n_datasets)
high_HR6 <- matrix(NA,nrow=25,ncol=n_datasets)


for (i in 1:n_datasets){
  result <- calc_naive_subgroup_estimates(base_model,subgroup_model,data=scenario6[[i]])
  naive_HR6[,i] <- result$exp.estimate
  low_HR6[, i] <- result$exp.conf.low
  high_HR6[, i] <- result$exp.conf.high
}

save(naive_HR6,file="Naive/naive_HR6.RData")
save(low_HR6, file="Naive/Conf/low_HR6.RData")
save(high_HR6, file="Naive/Conf/high_HR6.RData")


