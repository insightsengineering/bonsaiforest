#' Summary Naivepop Function
#'
#' Function to obtain the overall subgroup treatment effect of a
#' naivepop object.
#' @param object (`naivepop`)\cr the naivepop object.
#' @param ... \cr arguments of summary.
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
