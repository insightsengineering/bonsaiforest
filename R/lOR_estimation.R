#' Estimation of Log-Odds Ratio
#'
#' Function to estimate the subgroup log-odds ratio for binary data.
#'
#' @param x_subg (`matrix`)\cr the matrix with the subgroup covariates.
#' @param dummy_subg (`matrix`)\cr the dummy matrix with the subgroup
#'  covariates.
#' @param est_coef (`matrix`)\cr the estimated coefficients from the fitted
#'  model.
#'
#' @return Log-odds ratio of the studied subgroup.
#' @export
#'
#' @examples
#' lor_estimation(elastic_net_fit_bin$design_matrix,
#'  elastic_net_fit_bin$design_dummy, as.matrix(coef(elastic_net_fit_bin$fit,
#'  s = elastic_net_fit_bin$fit$lambda.min)))
lor_estimation <- function(x_subg, dummy_subg, est_coef) {
  assert_matrix(x_subg)
  assert_matrix(dummy_subg)
  assert_matrix(est_coef)
  x1 <- cbind(rep(1, nrow(x_subg)), rep(1, nrow(x_subg)),
              x_subg[, 2:(ncol(x_subg) - ncol(dummy_subg))], dummy_subg)
  x0 <- cbind(rep(1, nrow(x_subg)), rep(0, nrow(x_subg)),
              x_subg[, 2:(ncol(x_subg) - ncol(dummy_subg))], 0 * dummy_subg)
  prob <- function(x, est_coef) {
    k <- as.matrix(x) %*%  as.matrix(est_coef)
    p <- exp(k) / (1 + exp(k))
    y <- apply(p, 2, mean)
    y
  }
  y1 <- prob(x1, est_coef)
  y0 <- prob(x0, est_coef)
  phi <- as.numeric(log(y1 / (1 - y1)) - log(y0 / (1 - y0)))
  phi
}
