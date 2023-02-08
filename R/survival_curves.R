#' Average Survival Curves
#'
#' Function to obtain the average survival curve from all individual
#' survival curves.
#'
#' @param x (`matrix`)\cr the matrix with the subgroup covariates.
#' @param h0 (`numeric`)\cr the vector with the cumulative baseline hazard.
#' @param est_coef (`matrix`)\cr the estimated coefficients from the fitted
#' model.
#'
#' @return The survival probabilities at the sorted event times.
#' @export
#'
#' @examples
#' survival_curves(
#'   elastic_net_surv$x, elastic_net_surv$h0,
#'   elastic_net_surv$est_coef
#' )
survival_curves <- function(x, h0, est_coef) {
  assert_matrix(x)
  assert_numeric(h0)
  assert_matrix(est_coef)
  k2 <- -exp(x %*% est_coef)
  h0 <- as.matrix(h0)
  res <- surv_prob(k2, h0)[[1]]
  res
}
