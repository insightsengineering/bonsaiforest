#--------------------- functions to simulate data (survival data)


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


simul_pfs <- function(lp_aft,sigma_aft,recr_duration,rate_cens,n_events,  add_uncensored_pfs = F){
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
  # result
  result <- data.frame(tt_pfs=tt_pfs,ev_pfs=ev_pfs)
  if (add_uncensored_pfs == T) {
    result$tt_pfs_uncens <- tt_pfs_uncens
    result$ev_pfs_uncens <- 1
  }
  result
}

simul_tte_scenarios <- function(scenario, inflation_factor = 1, add_uncensored_pfs = F){
  n <- 1000 * inflation_factor
  n_events <- 247 * inflation_factor
  recr_duration <- 3 # 3 years recruitment
  rate_cens <- 0.02  # at 1 year

  #----------------- Simulate covariates
  covariates <- simul_covariates(n=n,p_catvar=10,add_contVars=F)

  #----------------- create design matrix with over-parametrized dummy coding for arm-subgroup interactions
  subgroup_model <- ~x_1+x_2+x_3+x_4+x_5+x_6+x_7+x_8+x_9+x_10

  if (scenario==6){ # mis-specified model including a x_1*x_2*arm interaction
    # create new variable combining x_1 and x_2
    covariates$x_1_2 <- factor(with(covariates,paste(as.character(x_1),as.character(x_2),sep="")))
    subgroup_model <- update(subgroup_model, ~.+x_1_2)
  }

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

  if (scenario==6){
    covariates$x_1_2 <- NULL # remove created variable again such that final covariates dataset is identical for all scenarios
  }

  #----------------- set regression coefficients (depending on scenario)
  reg_coef <- rep(0,ncol(design_matrix))
  names(reg_coef) <- colnames(design_matrix)

  # identical sigma_aft, intercept and prognostic factors as identical across scenarios
  sigma_aft <- 0.85
  reg_coef["(Intercept)"] <- 2
  reg_coef["x_4c"] <- -log(.7)*sigma_aft
  reg_coef["x_6b"] <- -log(1.5)*sigma_aft

  if (scenario==1){
    reg_coef["arm"] <- -log(0.66)*sigma_aft
  }

  if (scenario==2){
    reg_coef["arm"] <- -log(0.66)*sigma_aft
    reg_coef["x_4a_arm"] <- -reg_coef["arm"]       # no effect in x_4a
    reg_coef["x_4b_arm"] <- -log(0.8)*sigma_aft    # slightly enhanced efffect in x_4b and x_4c to "compensate" no effect in x_4a
    reg_coef["x_4c_arm"] <- -log(0.8)*sigma_aft    # slightly enhanced efffect in x_4b and x_4c to "compensate" no effect in x_4a
  }

  if (scenario==3){
    reg_coef["arm"] <- 0
    reg_coef["x_4a_arm"] <- -log(0.5)*sigma_aft
    reg_coef["x_4b_arm"] <- -log(1.25)*sigma_aft     # detrimental effect in x_4b and x_4c to "compensate" effect in x_4a
    reg_coef["x_4c_arm"] <- -log(1.25)*sigma_aft     # detrimental effect in x_4b and x_4c to "compensate" effect in x_4a
  }

  if (scenario==4){
    reg_coef["arm"] <- 0
    oldseed <- .GlobalEnv$.Random.seed # save old seed as scenario 4 parameters depend on a selected seed
    set.seed(5)
    log_hr_tmp <- rnorm(25,sd=0.15)
    reg_coef[which(names(reg_coef)=="x_1a_arm") : which(names(reg_coef)=="x_10c_arm")] <- -log_hr_tmp*sigma_aft
    .GlobalEnv$.Random.seed <- oldseed # get old seed back
  }

  if (scenario==5){
    reg_coef["arm"] <- 0
    oldseed <- .GlobalEnv$.Random.seed # save old seed as scenario 4 parameters depend on a selected seed
    set.seed(5)
    log_hr_tmp <- rnorm(25,sd=0.3)
    reg_coef[which(names(reg_coef)=="x_1a_arm") : which(names(reg_coef)=="x_10c_arm")] <- -log_hr_tmp*sigma_aft
    .GlobalEnv$.Random.seed <- oldseed # get old seed back
  }

  if (scenario==6){
    reg_coef["arm"] <- -log(0.66)*sigma_aft
    reg_coef["x_1_2aa_arm"] <- -log(1.5)*sigma_aft
    reg_coef["x_1_2ba_arm"] <- -log(0.5)*sigma_aft
    reg_coef["x_1_2ab_arm"] <- -log(0.92)*sigma_aft
    reg_coef["x_1_2bb_arm"] <- -log(1.07)*sigma_aft
  }

  #----------------- get linear predictor for AFT  and simulate corresponding outcome
  lp_aft <- design_matrix%*%reg_coef # linear predictor
  outcome <- simul_pfs(lp_aft = lp_aft,
                       sigma_aft = sigma_aft,
                       recr_duration = recr_duration,
                       rate_cens=rate_cens,
                       n_events = n_events,
                       add_uncensored_pfs = add_uncensored_pfs)
  d <- cbind(id=1:n,covariates,outcome)
  d
}

