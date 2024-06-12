#' Subgroup Treatment Effect Horseshoe
#'
#' Function to obtain the estimated posterior distribution of the subgroup
#' treatment effects considering a horseshoe fitted model.
#'
#' @param object (`horseshoe`)\cr the horseshoe object.
#' @param gamma (`scalar`)\cr numeric value defining the weights to obtain
#' the average hazard ratio. Default is 1 (in this case the average hazard
#' ratio obtained can be interpreted as the odds of concordance). Just needed
#' when using survival data.
#' @param l  (`scalar`)\cr the maximum value of time that wants to be studied to
#' obtain the average hazard ratio. Default is the maximum value of time when
#' there was an event. Just needed when using survival data.
#' @param m (`scalar`)\cr the value that defines the equally spaced time points
#' where the survival curves are going to be studied. Default is 50. Just needed
#' when using survival data.
#'
#' @return Approximated posterior distribution of the subgroup treatment effects.
#' @export
#'
#' @examples
#' trt_horseshoe(horseshoe_fit_surv, m = 1)
trt_horseshoe <- function(object, gamma = 1, l = NULL, m = 50) {
  assert_class(object, c("bonsaiforest", "horseshoe"))
  assert_int(m)
  x <- object$design_matrix
  resptype <- object$resptype
  fit_hs <- object$fit
  suppressWarnings(ms <- summary(fit_hs))
  iter <- ms$chains * (ms$iter - ms$warmup)
  trt_eff <- if (resptype == "binary") {
    est_coef <- t(as.matrix(fit_hs$fit)[seq_len(iter), seq_len(ncol(x) + 1)])
    subgroups(object, est_coef)
  } else if (resptype == "survival") {
    assert_scalar(gamma)
    y <- object$y
    est_coef <- t(as.matrix(fit_hs$fit)[seq_len(iter), seq_len(ncol(x))])
    sbhaz <- as.matrix(as.matrix(fit_hs$fit)[
      1:iter,
      c(
        "sbhaz[1]", "sbhaz[2]", "sbhaz[3]",
        "sbhaz[4]", "sbhaz[5]", "sbhaz[6]"
      )
    ])
    if (is.null(l)) {
      l <- max(y[which(y[, 2] == 1), 1])
    } else {
      assert_scalar(l)
    }

    resp_used <- seq(1, m) * l / m
    sort_resp <- sort(y[, 1])
    diff_resp <- min(sort_resp - c(0, sort_resp[-length(y[, 1])]))
    limits_resp <- c(max(min(y[, 1]) - diff_resp, 0), max(y[, 1]) + diff_resp)
    quantiles_resp <- stats::quantile(y[, 1], c(0.25, 0.5, 0.75))
    ispline <- splines2::iSpline(resp_used,
      Boundary.knots = limits_resp,
      knots = quantiles_resp, intercept = FALSE
    )
    h0 <- ispline %*% t(sbhaz)
    subgroups(object, est_coef, h0, gamma)
  }
}
