library(brms)
library(Rcpp)
library(splines2)
library(survival)


simul <- function(data) {
  #  Sys.setenv(PKG_CXXFLAGS="-std=c++14")
  data$arm <- as.factor(data$arm)
  horseshoe_fun <- function(resp, trt, subgr, covars, status = NULL, data) {
    preproc <- function(trt, subgr, covars, data) {
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
      colnames(design_ia) <- gsub(" ", "", colnames(design_ia)) # remove any spaces

      list(
        design_ia = design_ia, design_main = design_main,
        design_dummy = design_dummy, subgr_names = subgr_names
      )
    }

    #-----------------------------------------------------------------------------------------------------------

    prep_data <- preproc(trt, subgr, covars, data)
    design_matrix <- cbind(prep_data$design_main, prep_data$design_ia)
    form_b <- as.formula(paste("b ~ 0 +", paste0(colnames(prep_data$design_ia),
      collapse = " + "
    )))
    form_surv <- as.formula(paste(resp, "|cens(1-", status, ") ~ a + b"))
    form_a_surv <- as.formula(paste("a ~ 0 +", paste0(colnames(prep_data$design_main),
      collapse = " + "
    )))
    y <- as.data.frame(cbind(data[[resp]], data[[status]]))
    colnames(y) <- c(resp, status)
    data_model <- cbind(design_matrix, y)
    sort_resp <- sort(y[, 1])
    diff_resp <- min(sort_resp - c(0, sort_resp[-length(y[, 1])]))

    limits_resp <- c(max(min(y[, 1]) - diff_resp, 0), max(y[, 1]) + diff_resp)
    quantiles_resp <- quantile(y[, 1], c(0.25, 0.5, 0.75))
    bhaz <- list(
      Boundary.knots = limits_resp, knots = quantiles_resp,
      intercept = FALSE
    )
    fit_brms <- brm(bf(form_surv, nl = TRUE) + lf(form_a_surv) + lf(form_b),
      data = data_model, family = brmsfamily("cox", bhaz = bhaz),
      prior(normal(0, 5), class = "b", nlpar = "a") +
        prior(horseshoe(df = 1),
          class = "b", nlpar = "b"
        ),
      iter = 2000, warmup = 1000, chains = 4,
      control = list(adapt_delta = 0.95), seed = 0
    )

    list(
      fit = fit_brms, model = "horseshoe",
      data = data, design_matrix = design_matrix,
      design_dummy = prep_data$design_dummy, y = y, subgr_names = prep_data$subgr_names
    )
  }

  #----------------------------------------------------------------------------------------------------------------

  summary_horseshoe <- function(obj_hs) {
    #--------------------------------------------------------------------------------------------------------------
    trt_hs <- function(X, X_dummy, y, subgr_names, fit_hs, gamma = NULL, K, L = NULL) {
      # Values of coefficients of the model
      iter <- 4000

      est_coef <- t(as.matrix(fit_hs$fit)[1:iter, 1:(ncol(X))])
      sbhaz <- as.matrix(as.matrix(fit_hs$fit)[
        1:iter,
        c(
          "sbhaz[1]", "sbhaz[2]", "sbhaz[3]",
          "sbhaz[4]", "sbhaz[5]", "sbhaz[6]"
        )
      ])

      ### Response at events
      if (is.null(L)) {
        L <- max(y[which(y[, 2] == 1), 1])
      }

      sort_resp <- sort(y[, 1])
      diff_resp <- min(sort_resp - c(0, sort_resp[-length(y[, 1])]))
      resp_used <- seq(1, K) * L / K
      limits_resp <- c(max(min(y[, 1]) - diff_resp, 0), max(y[, 1]) + diff_resp)
      quantiles_resp <- quantile(y[, 1], c(0.25, 0.5, 0.75))
      ispline <- iSpline(resp_used,
        Boundary.knots = limits_resp,
        knots = quantiles_resp, intercept = FALSE
      )
      H0 <- ispline %*% t(sbhaz)


      # List with the variables that are going to be needed
      list <- list(H0 = H0, est_coef = est_coef, gamma = gamma)

      #------------------------------------------------------------------------------------------------------------
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

      #------------------------------------------------------------------------------------------------------------


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


      #--------------------------------------------------------------------------------------------

      subgroups <- function(X, X_dummy, subgr_names, list) {
        with(list, {
          trt_subg <- matrix(nrow = length(subgr_names), ncol = ncol(est_coef))
          i <- 1
          for (j in subgr_names) {
            X_subg <- X[which(X_dummy[, j] == 1), ]
            dummy_subg <- X_dummy[which(X_dummy[, j] == 1), ]
            trt_subg[i, ] <- AHR_funct(X_subg, dummy_subg, list)

            i <- i + 1
          }
          data.frame(subgroup = subgr_names, trt.estimate = trt_subg)
        })
      }

      # Call subgroups
      trt.eff <- subgroups(X, X_dummy, subgr_names, list)
      trt.eff
    }

    result <- trt_hs(obj_hs$design_matrix, obj_hs$design_dummy, obj_hs$y,
      obj_hs$subgr_names, obj_hs$fit,
      gamma = 1,
      K = 50
    )
    mean.trt <- apply(result[, -1], 1, mean)
    quant.trt <- apply(result[, -1], 1, quantile, prob = c(0.5, 0.025, 0.975))

    data.frame(
      subgroup = obj_hs$subgr_names, mean = mean.trt,
      median = quant.trt[1, ],
      low_cred = quant.trt[2, ],
      high_cred = quant.trt[3, ]
    )
  }

  model1 <- horseshoe_fun(
    resp = "tt_pfs", trt = "arm", subgr = c("x_1", "x_2", "x_3", "x_4", "x_5", "x_6", "x_7", "x_8", "x_9", "x_10"),
    covars = c(c("x_1", "x_2", "x_3", "x_4", "x_5", "x_6", "x_7", "x_8", "x_9", "x_10")),
    data = data, status = "ev_pfs"
  )
  result <- summary_horseshoe(model1)
  result
}

#--------------------------------------------------------------------------------------------------------------

### Set up of sHPC

## In the sHPC R interface you have to upload a zip folder (it will be directly unzipped)
## with the data sets of the scenarios.
## In this case we uploaded a folder in6 that inside has 1000 files of name data_i.rds
## Each one of these files contains one of the 1000 data sets generated in scenario 6.

library(batchtools)

reg <- makeRegistry("simulation_scenario6", packages = c("brms", "Rcpp", "splines2", "survival"))



batchExport(export = list(simul = simul))
batchMap(function(i) simul(readRDS(paste0("/home/vazqum37/in6/data_", i, ".rds"))),
  i = 1:1000
)

# submit the jobs
submitJobs(resources = list(walltime = 2000, memory = "30G", ncpus = 1))

getStatus()
waitForJobs()

result <- reduceResultsList()
result
