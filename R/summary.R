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
#' @return `trt_effect`
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
