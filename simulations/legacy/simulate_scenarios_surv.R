# Set parameters for the simulation study and obtain data for the different scenarios

library(tidyverse)
library(survival)
library(forestplot)


#--------------------------------------------------------------------------------------------------------------------------------------
# functions to simulate data

simul_covariates <- function(n,p_catvar=10,add_contVars=F){
  #-- generate a covariate matrix of n observations including treatment arm and p_catvar covariates
  #-- note: p_catvar leads to 25 subgroups from 10 variables
  # block diagonal covariance matrix for underlying multivariate normal data
  require(MASS)
  # create covariate matrix in blocks of 10
  Sigma <- matrix(0,nrow=10,ncol=10)
  Sigma[1:5,1:5] <- 0     # first 5 covariates uncorrelated with everything
  Sigma[6:8,6:8] <- 0.25   # cov 6-8 with "moderate" correlation
  Sigma[9:10,9:10] <- 0.5  # cov 9-10 with "high" correlation
  diag(Sigma) <- 1 # variance 1
  no_10_blocks <- ceiling(p_catvar/10)
  x <- NULL; z <- NULL
  for (j in 1:no_10_blocks){
    # continuous version
    z_j <- data.frame(mvrnorm(n,mu=rep(0,10),Sigma=Sigma))
    colnames(z_j) <- paste("z",(j-1)*10+1:10,sep="_")
    if (j==1) {z <- z_j} else { z <- cbind(z,z_j)}
    # categorized version
    x_j <- data.frame(v1=cut(z_j[,1],c(-Inf,qnorm(0.5),Inf),labels=c("a","b")),                       # 2 levels: 50:50
                      v2=cut(z_j[,2],c(-Inf,qnorm(0.4),Inf),labels=c("a","b")),
                      v3=cut(z_j[,3],c(-Inf,qnorm(0.2),Inf),labels=c("a","b")),
                      v4=cut(z_j[,4],c(-Inf,qnorm(c(0.3,0.6)),Inf),labels=c("a","b","c")),            # 3 levels: 30:30:40
                      v5=cut(z_j[,5],c(-Inf,qnorm(c(0.15,0.3,0.6)),Inf),labels=c("a","b","c","d")),   # 4 levels: 15:15:30:40
                      v6=cut(z_j[,6],c(-Inf,qnorm(0.4),Inf),labels=c("a","b")),
                      v7=cut(z_j[,7],c(-Inf,qnorm(0.4),Inf),labels=c("a","b")),
                      v8=cut(z_j[,8],c(-Inf,qnorm(c(0.2,0.5)),Inf),labels=c("a","b","c")),
                      v9=cut(z_j[,9],c(-Inf,qnorm(0.2),Inf),labels=c("a","b")),
                      v10=cut(z_j[,10],c(-Inf,qnorm(c(0.2,0.5)),Inf),labels=c("a","b","c")))
    colnames(x_j) <- paste("x",(j-1)*10+1:10,sep="_")
    if (j==1) {x <- x_j} else { x <- cbind(x,x_j)}
  }
  x <- cbind(arm=sample(rep(c(0,1),c(n%/%2,n-n%/%2))),x[,1:p_catvar])
  if (add_contVars) x <- cbind(x,z[,1:p_catvar])
  x
}

simul_pfs <- function(lp_aft,sigma_aft,recr_duration,rate_cens,n_events){
  n <- length(lp_aft)
  # Uncensored event time
  log_tt_pfs <- c(lp_aft+sigma_aft*log(rexp(n,rate=1)))
  tt_pfs_uncens <- exp(log_tt_pfs)
  # censoring 1: with rate_cens
  tt_pfs_cens1 <- rexp(n,rate=rate_cens)
  tt_pfs_cens1 <- pmin(tt_pfs_uncens,tt_pfs_cens1)
  ev_pfs_cens1 <- ifelse(tt_pfs_uncens<=tt_pfs_cens1,1,0)
  if (sum(ev_pfs_cens1)<n_events) stop(paste("Impossible to reach",n_events,"events with",n,"patients, a censoring rate of",rate_cens,"and the specified linear predictor."))
  # censoring 2: due to staggerred recruitment and recruiting only until target_ev events have been observed
  rec_time <- runif(n,min=0,max=recr_duration)
  tt_pfs_cens1_calendar <- rec_time+tt_pfs_cens1
  study_stop_time <- sort(tt_pfs_cens1_calendar[ev_pfs_cens1==1])[n_events]
  if (study_stop_time<max(rec_time)) warning("Target number of events reached before all subjects were enrolled.")
  tt_pfs <- pmax(0,pmin(tt_pfs_cens1_calendar,study_stop_time)-rec_time)
  ev_pfs <- ifelse(tt_pfs_cens1_calendar<=study_stop_time,ev_pfs_cens1,0)
  data.frame(tt_pfs=tt_pfs,ev_pfs=ev_pfs)
}

simul_data <- function(n,coef,sigma_aft,recr_duration,rate_cens,n_events){
  # Quickly simulate actual data combining functions covariates and simul_pfs (assuming 10 covariates)
  # Regression coefficients are for an AFT with over-parametrized dummy coding for arm-subgroup interactions (see creation of design matrix below)
  covariates <- simul_covariates(n=n,p_catvar=10,add_contVars=F)
  #- create design matrix with over-parametrized dummy coding for arm-subgroup interactions
  subgroup_model <- ~x_1+x_2+x_3+x_4+x_5+x_6+x_7+x_8+x_9+x_10
  design_main <- model.matrix(update(subgroup_model,~arm+.),data=covariates)
  subgroup_vars <- all.vars(subgroup_model)
  design_ia <- NULL
  for (j in subgroup_vars){
    ia_j <- model.matrix(as.formula(paste("~",j,"-1")),data=covariates)*covariates$arm
    design_ia <- cbind(design_ia,ia_j)
  }
  colnames(design_ia) <- paste(colnames(design_ia),"arm",sep="_")
  colnames(design_ia) <- gsub(" ","",colnames(design_ia)) # remove any spaces
  design_matrix <- cbind(design_main,design_ia)
  #- get linear predictor for AFT  and simulate corresponding outcome
  lp_aft <- design_matrix%*%coef # linear predictor
  outcome <- simul_pfs(lp_aft=lp_aft,sigma_aft=sigma_aft,recr_duration=recr_duration,rate_cens=rate_cens,n_events=n_events)
  d <- cbind(id=1:n,covariates,outcome)
  d
}

#-------------------------------------------------------------------------------------------------------------------------------------
# Create data for scenario 6

create_data <- function(n,coef,sigma_aft,recr_duration,rate_cens,n_events){
  covariates <- simul_covariates(n=n,p_catvar=10,add_contVars=F)
  #- create design matrix with over-parametrized dummy coding for arm-subgroup interactions
  subgroup_model <- ~x_1*x_2+x_3+x_4+x_5+x_6+x_7+x_8+x_9+x_10
  design_main <- model.matrix(update(subgroup_model,~arm+.),data=covariates)
  subgroup_vars <- all.vars(subgroup_model)
  design_ia <- NULL
  for (j in subgroup_vars){
    ia_j <- model.matrix(as.formula(paste("~",j,"-1")),data=covariates)*covariates$arm
    design_ia <- cbind(design_ia,ia_j)
  }
  design_ia <- cbind(design_ia, "x_1a2a"= design_ia[,1]*design_ia[,3], "x_1a2b"=design_ia[,1]*design_ia[,4], "x_1b2a"=design_ia[,2]*design_ia[,3], "x_1b2b"=design_ia[,2]*design_ia[,4] )
  colnames(design_ia) <- paste(colnames(design_ia),"arm",sep="_")
  colnames(design_ia) <- gsub(" ","",colnames(design_ia)) # remove any spaces
  design_matrix <- cbind(design_main,design_ia)
  #- get linear predictor for AFT  and simulate corresponding outcome
  lp_aft <- design_matrix%*%coef # linear predictor
  outcome <- simul_pfs(lp_aft=lp_aft,sigma_aft=sigma_aft,recr_duration=recr_duration,rate_cens=rate_cens,n_events=n_events)
  d <- cbind(id=1:n,covariates,outcome)
}

#--------------------------------------------------------------------------------------------------------------------------------------


### Simulation of scenarios

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
