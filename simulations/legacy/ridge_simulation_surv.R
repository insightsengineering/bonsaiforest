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
library(glmnet)
library(survival)
library(gbm)


### Obtain design and dummy matrix

preproc <- function(trt, subgr, covars, data) {
  levels(data[[trt]]) <- c("0", "1")
  covariates <- data[c(trt, covars)]
  subgr_model <- as.formula(paste("~ -1 +", paste(trt, "+"), paste0(covars, collapse = "+")))
  design_main <- model.matrix(subgr_model, data = covariates)
  colnames(design_main) <- gsub(" ", "", colnames(design_main))
  colnames(design_main) <- gsub("-", "", colnames(design_main))

  # Design matrix and dummy encoding
  design_ia <- NULL
  design_dummy <- NULL
  for (j in covars) {
    ia_j <- model.matrix(as.formula(paste("~", j, "-1")), data = covariates) * as.numeric(covariates[[trt]] == levels(covariates[[trt]])[2])
    design_ia <- cbind(design_ia, ia_j)
    ib_j <- model.matrix(as.formula(paste("~", j, "-1")), data = covariates)
    design_dummy <- cbind(design_dummy, ib_j)
  }
  colnames(design_dummy) <- gsub(" ", "", colnames(design_dummy))
  colnames(design_dummy) <- gsub("-", "", colnames(design_dummy))

  subgr_names <- NULL
  for (k in subgr) {
    ic_k <- model.matrix(as.formula(paste("~", k, "-1")), data = covariates)
    subgr_names <- c(subgr_names, colnames(ic_k))
  }
  subgr_names <- gsub(" ", "", subgr_names)

  colnames(design_ia) <- paste(colnames(design_ia), trt, sep = "_")
  colnames(design_ia) <- gsub(" ", "", colnames(design_ia))
  colnames(design_ia) <- gsub("-", "", colnames(design_ia))

  list(
    design_ia = design_ia, design_main = design_main,
    design_dummy = design_dummy, subgr_names = subgr_names
  )
}

### Fit model

elastic_net <- function(resp, trt, subgr, covars, status = NULL, data, resptype, alpha) {
  prep_data <- preproc(trt, subgr, covars, data)
  if (resptype == "survival") {
    penalty_factor <- c(
      rep(0, ncol(prep_data$design_main)),
      rep(1, ncol(prep_data$design_ia))
    )
    design_matrix <- cbind(prep_data$design_main, prep_data$design_ia)
    fit_glmnet <- cv.glmnet(design_matrix, Surv(data[[resp]], data[[status]]),
      family = "cox", penalty.factor = penalty_factor,
      alpha = alpha
    )
    y <- as.data.frame(cbind(data[[resp]], data[[status]]))
    colnames(y) <- c("resp", "status")
  } else if (resptype == "binary") {
    penalty_factor <- c(
      rep(0, ncol(prep_data$design_main[, -1])),
      rep(1, ncol(prep_data$design_ia))
    )
    design_matrix <- cbind(prep_data$design_main[, -1], prep_data$design_ia)
    fit_glmnet <- cv.glmnet(design_matrix, data[[resp]],
      family = "binomial",
      penalty.factor = penalty_factor, alpha = alpha
    )
    y <- data[[resp]]
  }
  list(
    fit = fit_glmnet, alpha = alpha, model = "elastic_net",
    data = data, resptype = resptype, design_matrix = design_matrix,
    design_dummy = prep_data$design_dummy, y = y, subgr_names = prep_data$subgr_names
  )
}

### Obtain summary of the model

summary_en <- function(obj_en) {
  if (obj_en$resptype == "survival") {
    result <- trt_en(obj_en$design_matrix, obj_en$design_dummy, obj_en$y,
      obj_en$subgr_names, obj_en$resptype, obj_en$fit,
      gamma = 1
    )
  } else if (obj_en$resptype == "binary") {
    result <- trt_en(
      obj_en$design_matrix, obj_en$design_dummy, obj_en$y,
      obj_en$subgr_names, obj_en$resptype, obj_en$fit
    )
  }
  result
}

### Helper functions for the summary of the model

trt_en <- function(X, X_dummy, y, subgr_names, resptype, fit, gamma = NULL, L = NULL) {
  # Values of coefficients of the model
  lambda <- fit$lambda.min
  est_coef <- coef(fit, s = lambda)

  if (resptype == "binary") {
    list <- list(est_coef = est_coef)
    trt.eff <- subgroups(X, X_dummy, subgr_names, resptype, list)
  } else if (resptype == "survival") {
    # Find unique sorted time points
    order_resp <- order(y$resp)
    resp_un <- y$resp[order_resp]

    ### Some important variables
    lp <- predict(fit, lambda, newx = as.matrix(X))
    lp_un <- lp[order_resp]
    status_un <- y$status[order_resp]


    ### Cumulative baseline hazard at some points
    bh <- basehaz.gbm(
      t = resp_un, delta = status_un, f.x = lp_un, t.eval = resp_un,
      smooth = TRUE, cumulative = TRUE
    )

    ### Response at events
    resp_ev <- resp_un[which(status_un == 1)]
    if (is.null(L)) {
      L <- max(resp_ev)
    }

    ### index of times with ev=1 and resp<=L
    ind_time <- which(status_un == 1 & resp_un <= L)


    # List with the variables that are going to be needed
    list <- list(
      H0 = bh[ind_time], est_coef = est_coef,
      gamma = gamma
    )

    # Call subgroups
    trt.eff <- subgroups(X, X_dummy, subgr_names, resptype, list)
  }
  trt.eff
}


subgroups <- function(X, X_dummy, subgr_names, resptype, list) {
  with(list, {
    trt_subg <- matrix(nrow = length(subgr_names), ncol = ncol(est_coef))
    i <- 1
    for (j in subgr_names) {
      X_subg <- X[which(X_dummy[, j] == 1), ]
      dummy_subg <- X_dummy[which(X_dummy[, j] == 1), ]
      if (resptype == "survival") {
        trt_subg[i, ] <- AHR_funct(X_subg, dummy_subg, list)
      } else if (resptype == "binary") {
        trt_subg[i, ] <- lOR_funct(X_subg, dummy_subg, est_coef)
      }
      i <- i + 1
    }
    data.frame(subgroup = subgr_names, trt.estimate = trt_subg)
  })
}


AHR_funct <- function(X_subg, dummy_subg, list) {
  with(list, {
    # Data for treatment
    X1 <- cbind(
      rep(0, nrow(X_subg)), rep(1, nrow(X_subg)),
      X_subg[, 3:(ncol(X_subg) - ncol(dummy_subg))], dummy_subg
    )

    # Data for control
    X0 <- cbind(
      rep(1, nrow(X_subg)), rep(0, nrow(X_subg)),
      X_subg[, 3:(ncol(X_subg) - ncol(dummy_subg))], 0 * dummy_subg
    )

    # Apply function to the trt and contr data
    res_1 <- curves_data(X1, list)
    res_0 <- curves_data(X0, list)

    # Num and den of the formula of AHR_OC

    N <- nrow(res_1)
    num <- apply(res_0^gamma * (res_1^gamma - rbind(1, as.data.frame(res_1[-N, ]))^gamma), 2, sum)
    den <- apply(res_1^gamma * (res_0^gamma - rbind(1, as.data.frame(res_0[-N, ]))^gamma), 2, sum)

    # Value of AHR_OC
    AHR_OC <- num / den

    AHR_OC
  })
}


curves_data <- function(X, list) {
  with(list, {
    # Constant (exp of the linear combination of the data with the coef of the model)
    K2 <- -exp(as.matrix(X) %*% as.matrix(est_coef))
    H0 <- as.matrix(H0)

    # Survival curves with function
    library(Rcpp)
    library(RcppArmadillo)
    cppFunction(depends = "RcppArmadillo", "List surv_prob(arma::mat K, arma::mat H){
    NumericMatrix surv(H.n_rows, H.n_cols);
    for (int i = 0; i < H.n_cols; i++){
      arma::mat h1(K.n_rows, H.n_rows);
      arma::mat x1 = K.col(i)*(H.col(i).t());
      arma::mat x2 = exp(x1);
      NumericMatrix x3 = wrap(x2);
      surv(_,i) = colMeans(x3);
   }
    return List::create(surv);

  }")

    res <- surv_prob(K2, H0)[[1]]
    res
  })
}



### --------------------------------------------------------------------------------------------------------------------

### Simulations:

library(tidyverse)
library(survival)


# number of simulated datasets
n_datasets <- 1000


## Ridge HR: scenario1
set.seed(0)

ridge1_HR1 <- matrix(NA, nrow = 25, ncol = n_datasets)

for (i in 1:n_datasets) {
  data1 <- scenario1[[i]]
  data1$arm <- as.factor(data1$arm)
  fit_result <- elastic_net(
    resp = "tt_pfs", trt = "arm",
    subgr = c("x_1", "x_2", "x_3", "x_4", "x_5", "x_6", "x_7", "x_8", "x_9", "x_10"),
    covars = c("x_1", "x_2", "x_3", "x_4", "x_5", "x_6", "x_7", "x_8", "x_9", "x_10"),
    data = data1, resptype = "survival", status = "ev_pfs",
    alpha = 0
  )
  result <- summary_en(fit_result)[, 2]
  ridge1_HR1[, i] <- result
}

save(ridge1_HR1, file = "Ridge/ridge1_HR1.RData")


## Ridge HR: scenario2
set.seed(0)

ridge2_HR1 <- matrix(NA, nrow = 25, ncol = n_datasets)

for (i in 1:n_datasets) {
  data1 <- scenario2[[i]]
  data1$arm <- as.factor(data1$arm)
  fit_result <- elastic_net(
    resp = "tt_pfs", trt = "arm",
    subgr = c("x_1", "x_2", "x_3", "x_4", "x_5", "x_6", "x_7", "x_8", "x_9", "x_10"),
    covars = c("x_1", "x_2", "x_3", "x_4", "x_5", "x_6", "x_7", "x_8", "x_9", "x_10"),
    data = data1, resptype = "survival", status = "ev_pfs",
    alpha = 0
  )
  result <- summary_en(fit_result)[, 2]
  ridge2_HR1[, i] <- result
}

save(ridge2_HR1, file = "Ridge/ridge2_HR1.RData")


## Ridge HR: scenario3
set.seed(0)

ridge3_HR1 <- matrix(NA, nrow = 25, ncol = n_datasets)

for (i in 1:n_datasets) {
  data1 <- scenario3[[i]]
  data1$arm <- as.factor(data1$arm)
  fit_result <- elastic_net(
    resp = "tt_pfs", trt = "arm",
    subgr = c("x_1", "x_2", "x_3", "x_4", "x_5", "x_6", "x_7", "x_8", "x_9", "x_10"),
    covars = c("x_1", "x_2", "x_3", "x_4", "x_5", "x_6", "x_7", "x_8", "x_9", "x_10"),
    data = data1, resptype = "survival", status = "ev_pfs",
    alpha = 0
  )
  result <- summary_en(fit_result)[, 2]
  ridge3_HR1[, i] <- result
}

save(ridge3_HR1, file = "Ridge/ridge3_HR1.RData")


## Ridge HR: scenario4
set.seed(0)

ridge4_HR1 <- matrix(NA, nrow = 25, ncol = n_datasets)

for (i in 1:n_datasets) {
  data1 <- scenario4[[i]]
  data1$arm <- as.factor(data1$arm)
  fit_result <- elastic_net(
    resp = "tt_pfs", trt = "arm",
    subgr = c("x_1", "x_2", "x_3", "x_4", "x_5", "x_6", "x_7", "x_8", "x_9", "x_10"),
    covars = c("x_1", "x_2", "x_3", "x_4", "x_5", "x_6", "x_7", "x_8", "x_9", "x_10"),
    data = data1, resptype = "survival", status = "ev_pfs",
    alpha = 0
  )
  result <- summary_en(fit_result)[, 2]
  ridge4_HR1[, i] <- result
}

save(ridge4_HR1, file = "Ridge/ridge4_HR1.RData")


## Ridge HR: scenario5
set.seed(0)

ridge5_HR1 <- matrix(NA, nrow = 25, ncol = n_datasets)

for (i in 1:n_datasets) {
  data1 <- scenario5[[i]]
  data1$arm <- as.factor(data1$arm)
  fit_result <- elastic_net(
    resp = "tt_pfs", trt = "arm",
    subgr = c("x_1", "x_2", "x_3", "x_4", "x_5", "x_6", "x_7", "x_8", "x_9", "x_10"),
    covars = c("x_1", "x_2", "x_3", "x_4", "x_5", "x_6", "x_7", "x_8", "x_9", "x_10"),
    data = data1, resptype = "survival", status = "ev_pfs",
    alpha = 0
  )
  result <- summary_en(fit_result)[, 2]
  ridge5_HR1[, i] <- result
}

save(ridge5_HR1, file = "Ridge/ridge5_HR1.RData")


## Ridge HR: scenario6
set.seed(0)

ridge6_HR1 <- matrix(NA, nrow = 25, ncol = n_datasets)

for (i in 1:n_datasets) {
  data1 <- scenario6[[i]]
  data1$arm <- as.factor(data1$arm)
  fit_result <- elastic_net(
    resp = "tt_pfs", trt = "arm",
    subgr = c("x_1", "x_2", "x_3", "x_4", "x_5", "x_6", "x_7", "x_8", "x_9", "x_10"),
    covars = c("x_1", "x_2", "x_3", "x_4", "x_5", "x_6", "x_7", "x_8", "x_9", "x_10"),
    data = data1, resptype = "survival", status = "ev_pfs",
    alpha = 0
  )
  result <- summary_en(fit_result)[, 2]
  ridge6_HR1[, i] <- result
}

save(ridge6_HR1, file = "Ridge/ridge6_HR1.RData")
