#' Subgroup Treatment Effect
#'
#' Function to obtain the estimated treatment effect in each one of the
#' considered subgroups.
#'
#' @param object (`bonsaiforest`)\cr the fitted bonsaiforest object.
#' @param est_coef (`matrix`)\cr the estimated coefficients from the fitted
#' model.
#' @param h0 (`numeric`)\cr the vector with the cumulative baseline hazard.
#' Present just for `resptype` survival.
#' @param gamma (`scalar`)\cr numeric value defining the weights to obtain
#' the average hazard ratio. Default is 1 (in this case the average hazard
#' ratio obtained can be interpreted as the odds of concordance).
#'
#' @return `data.frame` with the subgroup names and with the estimated subgroup
#' treatment effects.
#' @export
#'
#' @examples
#' subgroups(
#'   elastic_net_fit_surv, elastic_net_surv$est_coef,
#'   elastic_net_surv$h0
#' )
subgroups <- function(object, est_coef, h0 = NULL, gamma = 1) {
  assert_class(object, "bonsaiforest")
  assert_matrix(est_coef)
  x <- object$design_matrix
  x_dummy <- object$design_dummy
  subgr_names <- object$subgr_names
  resptype <- object$resptype
  trt_subg <- matrix(nrow = length(subgr_names), ncol = ncol(est_coef))
  i <- 1
  for (j in subgr_names) {
    x_subg <- x[which(x_dummy[, j] == 1), ]
    dummy_subg <- x_dummy[which(x_dummy[, j] == 1), ]
    if (resptype == "survival") {
      assert_numeric(h0)
      assert_scalar(gamma)
      trt_subg[i, ] <- ahr_estimation(x_subg, dummy_subg, est_coef, h0, gamma)
    } else if (resptype == "binary") {
      trt_subg[i, ] <- lor_estimation(x_subg, dummy_subg, est_coef)
    }
    i <- i + 1
  }
  data.frame(subgroup = subgr_names, trt.estimate = trt_subg)
}
