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
#' lor_estimation(design_matrix1, design_dummy1, est_coef_bin1)
lor_estimation <- function(x_subg, dummy_subg, est_coef) {
  assert_matrix(x_subg)
  assert_matrix(dummy_subg)
  assert_matrix(est_coef)
  x_arm <- list()
  for (i in 0:1) {
    x_arm[[i + 1]] <- cbind(
      rep(1, nrow(x_subg)), rep(i, nrow(x_subg)),
      x_subg[, 2:(ncol(x_subg) - ncol(dummy_subg))], i * dummy_subg
    )
  }
  prob <- function(x, est_coef) {
    k <- as.matrix(x) %*% as.matrix(est_coef)
    p <- exp(k) / (1 + exp(k))
    y <- apply(p, 2, mean)
    y
  }
  y_arm <- lapply(x_arm, prob, est_coef = est_coef)
  phi <- as.numeric(
    log(y_arm[[2]] / (1 - y_arm[[2]])) -
      log(y_arm[[1]] / (1 - y_arm[[1]]))
  )
  phi
}
