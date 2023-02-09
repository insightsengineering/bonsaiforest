#' Average Hazard Ratio Estimation
#'
#' Function to obtain the average hazard ratio.
#'
#' @param x_subg (`matrix`)\cr the matrix with the subgroup covariates.
#' @param dummy_subg (`matrix`)\cr the dummy matrix with the subgroup
#'  covariates.
#' @param est_coef (`matrix`)\cr the estimated coefficients from the fitted
#'  model.
#' @param h0 (`numeric`)\cr the vector with the cumulative baseline hazard.
#' @param gamma (`scalar`)\cr numeric value defining the weights to obtain
#' the average hazard ratio. Default is 1 (in this case the average hazard
#' ratio obtained can be interpreted as the odds of concordance).
#'
#' @return Average hazard ratio
#' @export
#'
#' @examples
#' ahr_estimation(
#'   elastic_net_surv$design1, elastic_net_surv$dummy1,
#'   elastic_net_surv$est_coef, elastic_net_surv$h0
#' )
ahr_estimation <- function(x_subg, dummy_subg, est_coef, h0, gamma = 1) {
  assert_matrix(x_subg)
  assert_matrix(dummy_subg)
  assert_matrix(est_coef)
  assert_numeric(h0)
  assert_scalar(gamma)
  res <- list()
  for (i in 0:1) {
    x <- cbind(
      rep(abs(i - 1), nrow(x_subg)), rep(i, nrow(x_subg)),
      x_subg[, 3:(ncol(x_subg) - ncol(dummy_subg))], i * dummy_subg
    )
    res[[i + 1]] <- survival_curves(x, h0, est_coef)
  }
  n <- nrow(res[[1]])
  frac <- list()
  for (i in 1:2){
    j <- setdiff(c(1, 2), i)
    frac[[i]] <- apply(res[[i]]^gamma *
                         (res[[j]]^gamma - rbind(1, as.data.frame(res[[j]][-n, ]))^gamma),
                       2, sum)
  }
  ahr <- as.numeric(frac[[1]] / frac[[2]])
  ahr
}
