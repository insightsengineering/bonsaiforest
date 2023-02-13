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
#' ratio obtained can be interpreted as the odds of concordance). Just needed
#' when using survival data.
#' @param l (`scalar`)\cr the maximum value of time that wants to be studied to
#' obtain the average hazard ratio. Default is the maximum value of time when
#' there was an event. Just needed when using survival data.
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
  trt_eff <- if (resptype == "binary") {
    subgroups(object, est_coef)
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
      l <- max(resp_ev)
    } else {
      assert_scalar(l)
    }
    ind_time <- which(status_un == 1 & resp_un <= l)
    h0 <- bh[ind_time]
    subgroups(object, est_coef, h0, gamma)
  }
  trt_eff
}


#' Summary Horseshoe Function
#'
#' Function to obtain the estimated subgroup treatment effects from a `horseshoe`
#' model and a credible interval for them.
#'
#' @param object (`horseshoe`)\cr the horseshoe object.
#' @param conf (`scalar`)\cr the level of the credible intervals. Default is
#' 0.95.
#' @param gamma (`scalar`)\cr numeric value defining the weights to obtain
#' the average hazard ratio. Default is 1 (in this case the average hazard
#' ratio obtained can be interpreted as the odds of concordance). Just needed
#' when using survival data.
#' @param l (`scalar`)\cr the maximum value of time that wants to be studied to
#' obtain the average hazard ratio. Default is the maximum value of time when
#' there was an event. Just needed when using survival data.
#' @param m (`scalar`)\cr the value that defines the equally spaced time points
#' where the survival curves are going to be studied. Default is 50. Just needed
#' when using survival data.
#' @param ... Arguments of summary.
#'
#' @return Object of class `summary.horseshoe` which is a `list` with the
#'  approximated posterior distribution of the treatment
#'  effects and a `data.frame` with the estimated subgroup treatment effect
#'  (with the median) and the bounds of the credible intervals.
#'
#' @export
#'
#' @examples
#' summary(horseshoe_fit_bin)
summary.horseshoe <- function(object, conf = 0.95, gamma = 1, l = NULL, m = 50, ...) {
  assert_class(object, c("shrinkforest", "horseshoe"))
  assert_scalar(conf)
  assert_scalar(gamma)
  assert_int(m)
  if (!is.null(l)) {
    assert_scalar(l)
  }
  alpha <- 1 - conf
  result <- trt_horseshoe(object, gamma, l, m)
  trt_quant <- apply(result[, -1], 1, stats::quantile,
    prob = c(0.5, alpha / 2, 1 - alpha / 2)
  )
  summary_post <- data.frame(
    subgroup = object$subgr_names,
    trt.median = trt_quant[1, ],
    trt.cred.low = trt_quant[2, ],
    trt.cred.high = trt_quant[3, ]
  )
  result <- list(posterior = result, summary_post = summary_post)
  class(result) <- "summary.horseshoe"
  return(result)
}
