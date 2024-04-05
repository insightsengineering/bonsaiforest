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

### Helper functions
library(survival)

HR_population <- function(data){
  model <- coxph(Surv(tt_pfs,ev_pfs)~arm,data=data)
  HR <- exp(coef(model))
  return(HR)
}

conf_pop <- function(data){
  model <- coxph(Surv(tt_pfs,ev_pfs)~arm,data=data)
  conf <- summary(model)$conf.int[, c(3,4)]
  return(conf)
}


###-----------------------------------------------------------------------------------------------

### Simulations:

# number of simulated datasets
n_datasets <- 1000


## Population HR: scenario1
set.seed(0)

pop_HR1 <- matrix(NA,nrow=25,ncol=n_datasets)
low_HR1p <- matrix(NA,nrow=25,ncol=n_datasets)
high_HR1p <- matrix(NA,nrow=25,ncol=n_datasets)

for (i in 1:n_datasets){
  result <- HR_population(scenario1[[i]])
  conf <- conf_pop(scenario1[[i]])
  low_HR1p[,i] <- conf[1]
  high_HR1p[,i] <- conf[2]
  pop_HR1[,i] <- result
}

save(pop_HR1, file="Population/pop_HR1.RData")
save(low_HR1p, file="Population/Conf/low_HR1p.RData")
save(high_HR1p, file="Population/Conf/high_HR1p.RData")


## Population HR: scenario2
set.seed(0)

pop_HR2 <- matrix(NA,nrow=25,ncol=n_datasets)
low_HR2p <- matrix(NA,nrow=25,ncol=n_datasets)
high_HR2p <- matrix(NA,nrow=25,ncol=n_datasets)

for (i in 1:n_datasets){
  result <- HR_population(scenario2[[i]])
  conf <- conf_pop(scenario2[[i]])
  low_HR2p[,i] <- conf[1]
  high_HR2p[,i] <- conf[2]
  pop_HR2[,i] <- result
}

save(pop_HR2, file="Population/pop_HR2.RData")
save(low_HR2p, file="Population/Conf/low_HR2p.RData")
save(high_HR2p, file="Population/Conf/high_HR2p.RData")


## Population HR: scenario3
set.seed(0)

pop_HR3 <- matrix(NA,nrow=25,ncol=n_datasets)
low_HR3p <- matrix(NA,nrow=25,ncol=n_datasets)
high_HR3p <- matrix(NA,nrow=25,ncol=n_datasets)

for (i in 1:n_datasets){
  result <- HR_population(scenario3[[i]])
  conf <- conf_pop(scenario3[[i]])
  low_HR3p[,i] <- conf[1]
  high_HR3p[,i] <- conf[2]
  pop_HR3[,i] <- result
}

save(pop_HR3, file="Population/pop_HR3.RData")
save(low_HR3p, file="Population/Conf/low_HR3p.RData")
save(high_HR3p, file="Population/Conf/high_HR3p.RData")


## Population HR: scenario4
set.seed(0)

pop_HR4 <- matrix(NA,nrow=25,ncol=n_datasets)
low_HR4p <- matrix(NA,nrow=25,ncol=n_datasets)
high_HR4p <- matrix(NA,nrow=25,ncol=n_datasets)

for (i in 1:n_datasets){
  result <- HR_population(scenario4[[i]])
  conf <- conf_pop(scenario4[[i]])
  low_HR4p[,i] <- conf[1]
  high_HR4p[,i] <- conf[2]
  pop_HR4[,i] <- result
}

save(pop_HR4, file="Population/pop_HR4.RData")
save(low_HR4p, file="Population/Conf/low_HR4p.RData")
save(high_HR4p, file="Population/Conf/high_HR4p.RData")


## Population HR: scenario5
set.seed(0)

pop_HR5 <- matrix(NA,nrow=25,ncol=n_datasets)
low_HR5p <- matrix(NA,nrow=25,ncol=n_datasets)
high_HR5p <- matrix(NA,nrow=25,ncol=n_datasets)

for (i in 1:n_datasets){
  result <- HR_population(scenario5[[i]])
  conf <- conf_pop(scenario5[[i]])
  low_HR5p[,i] <- conf[1]
  high_HR5p[,i] <- conf[2]
  pop_HR5[,i] <- result
}

save(pop_HR5, file="Population/pop_HR5.RData")
save(low_HR5p, file="Population/Conf/low_HR5p.RData")
save(high_HR5p, file="Population/Conf/high_HR5p.RData")


## Population HR: scenario6
set.seed(0)

pop_HR6 <- matrix(NA,nrow=25,ncol=n_datasets)
low_HR6p <- matrix(NA,nrow=25,ncol=n_datasets)
high_HR6p <- matrix(NA,nrow=25,ncol=n_datasets)

for (i in 1:n_datasets){
  result <- HR_population(scenario6[[i]])
  conf <- conf_pop(scenario6[[i]])
  low_HR6p[,i] <- conf[1]
  high_HR6p[,i] <- conf[2]
  pop_HR6[,i] <- result
}

save(pop_HR6, file="Population/pop_HR6.RData")
save(low_HR6p, file="Population/Conf/low_HR6p.RData")
save(high_HR6p, file="Population/Conf/high_HR6p.RData")
