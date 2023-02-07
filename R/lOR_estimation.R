#' Estimation of Log-Odds Ratio
#'
#' Function to estimate the subgroup log-odds ratio for binary data.
#'
#' @param X_subg (`matrix`)\cr the matrix with the subgroup covariates.
#' @param dummy_subg (`matrix`)\cr the dummy matrix with the subgroup
#'  covariates.
#' @param est_coef (`matrix`)\cr the estimated coefficients from the fitted
#'  model.
#'
#' @return Log-odds ratio of the studied subgroup.
#' @export
#'
#' @examples
#' lOR_estimation(elastic_net_fit_bin$design_matrix,
#'  elastic_net_fit_bin$design_dummy, as.matrix(coef(elastic_net_fit_bin$fit,
#'  s = elastic_net_fit_bin$fit$lambda.min)))
lOR_estimation <- function(X_subg, dummy_subg, est_coef) {
  assert_matrix(X_subg)
  assert_matrix(dummy_subg)
  assert_matrix(est_coef)
  X1 <- cbind(rep(1, nrow(X_subg)), rep(1, nrow(X_subg)),
              X_subg[, 2:(ncol(X_subg) - ncol(dummy_subg))], dummy_subg)
  X0 <- cbind(rep(1, nrow(X_subg)), rep(0, nrow(X_subg)),
              X_subg[, 2:(ncol(X_subg) - ncol(dummy_subg))], 0 * dummy_subg)
  prob <- function(X, est_coef) {
    K <- as.matrix(X) %*%  as.matrix(est_coef)
    P <- exp(K) / (1 + exp(K))
    Y <- apply(P, 2, mean)
    Y
  }
  Y1 <- prob(X1, est_coef)
  Y0 <- prob(X0, est_coef)
  phi <- as.numeric(log(Y1 / (1 - Y1)) - log(Y0 / (1 - Y0)))
  phi
}
