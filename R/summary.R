#' Summary Naivepop Function
#'
#' Function to obtain the overall subgroup treatment effect of a
#' naivepop object.
#' @param object (`naivepop`)\cr the naivepop object.
#' @param ... Arguments of summary.
#'
#' @return The estimated overall treatment effects.
#' @export
#'
#' @examples
#' summary(naivepop_fit_surv)
summary.naivepop <- function(object, ...) {
  assert_class(object, c("shrinkforest", "naivepop"))
  if (object$resptype == "survival") {
    trt_effect <- exp(stats::coef(object$fit))
  } else if (object$resptype == "binary") {
    trt_effect <- stats::coef(object$fit)[2]
  }
  trt_effect
}


#' Summary Naive
#'
#' Function to obtain the naive subgroup treatment effects of an object fitted
#' with the `naive` function.
#' @param object (`naive`)\cr the naive object.
#' @param conf (`scalar`)\cr the confidence level of the intervals. Default is
#' 0.95.
#' @param ... Arguments of summary.
#'
#' @return A `data.frame` with 4 columns: the subgroup variables, the estimated
#' treatment effect and the low and high bounds of the confidence interval of the
#' treatment effect.
#' @export
#'
#' @examples
#' summary(naive_fit_surv)
summary.naive <- function(object, conf = 0.95, ...) {
  assert_class(object, c("shrinkforest", "naive"))
  assert_scalar(conf)
  alpha <- 1 - conf
  estim <- object$estimates
  estim$trt.effect <- estim$estimate
  estim$trt.conf.low <- estim$estimate + stats::qnorm(alpha / 2) * estim$std.error
  estim$trt.conf.high <- estim$estimate - stats::qnorm(alpha / 2) * estim$std.error
  if (object$resptype == "survival") {
    estim[, c(7:9)] <- exp(estim[, c(7:9)])
  }
  estim[, c(1, 7:9)]
}


#' Summary Elastic Net Function
#'
#' Function to obtain the naive subgroup treatment effects of an object fitted
#' with the `elastic_net` function.
#'
#' @param object (`elastic_net`)\cr the elastic_net object.
#' @param gamma (`scalar`)\cr numeric value defining the weights to obtain
#' the average hazard ratio. Default is 1 (in this case the average hazard
#' ratio obtained can be interpreted as the odds of concordance).
#' @param L (`scalar`)\cr the maximum value of time that wants to be studied to
#' obtain the average hazard ratio. Default is the maximum value of time when
#' there was an event.
#' @param lambda (`scalar`)\cr the penalization constant in the elastic net.
#' Default is the value that leads to minimal cross validation error.
#' @param ... Arguments of summary
#'
#' @return `data.frame` with the subgroup names and with the estimated subgroup
#' treatment effects.
#' @export
#'
#' @examples
#' summary(elastic_net_fit_surv)
summary.elastic_net <- function(object, gamma = 1, l = NULL, lambda = NULL, ...) {
  assert_class(object, c("shrinkforest", "elastic_net"))
  x <- object$design_matrix
  resptype <- object$resptype
  fit <- object$fit
  assert_scalar(gamma)
  if (is.null(lambda)) {
    lambda <- fit$lambda.min
  } else {
    assert_scalar(lambda)
  }
  est_coef <- as.matrix(stats::coef(fit, s = lambda))
  if (resptype == "binary") {
    trt_eff <- subgroups(object, est_coef)
  } else if (resptype == "survival") {
    y <- object$y
    assert_data_frame(y)
    order_resp <- order(y$resp)
    resp_un <- y$resp[order_resp]
    lp <- stats::predict(fit, lambda, newx = as.matrix(x))
    lp_un <- lp[order_resp]
    status_un <- y$status[order_resp]
    bh <- gbm::basehaz.gbm(
      t = resp_un, delta = status_un, f.x = lp_un,
      t.eval = resp_un, smooth = TRUE, cumulative = TRUE
    )
    resp_ev <- resp_un[which(status_un == 1)]
    if (is.null(l)) {
      L <- max(resp_ev)
    } else {
      assert_scalar(l)
    }
    ind_time <- which(status_un == 1 & resp_un <= l)
    h0 <- bh[ind_time]
    trt_eff <- subgroups(object, est_coef, h0, gamma)
  }
  trt_eff
}
