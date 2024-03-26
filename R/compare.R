#' Compare Treatment Estimate Methods
#'
#' Function to create a data set with the results for all the methods in order
#' to compare them.
#'
#' @param ... Objects of class `bonsaiforest`. We write them separated by a comma.
#'
#' @return List with a `data.frame` of the estimated subgroup treatment effects,
#' with the overall treatment effect and with the response type of our data.
#' @export
#'
#' @examples
#' compare(naivepop_fit_surv, naive_fit_surv, elastic_net_fit_surv)
compare <- function(...) {
  list_objects <- list(...)
  if (length(list_objects) < 2) {
    stop("There should be at least two models to compare")
  }
  data <- NULL
  overall_trt <- NULL
  for (obj in list_objects) {
    assert_class(obj, "bonsaiforest")
    sum_obj <- summary(obj)
    if (inherits(sum_obj, "summary.naivepop")) {
      assert_class(sum_obj, "summary.naivepop")
      overall_trt <- as.numeric(sum_obj$estimates)
    }
    data <- if (inherits(sum_obj, "summary.naive")) {
      rbind(data, data.frame(sum_obj$estimates,
        model = rep("Naive", nrow(sum_obj$estimates))
      ))
    } else if (inherits(sum_obj, "summary.elastic_net")) {
      rbind(data, data.frame(sum_obj$estimates,
        trt.low = sum_obj$estimates[, 2],
        trt.high = sum_obj$estimates[, 2],
        model = rep(
          paste("Elastic net alpha =", sum_obj$alpha),
          nrow(sum_obj$estimates)
        )
      ))
    } else if (inherits(sum_obj, "summary.horseshoe")) {
      assert_class(sum_obj, "summary.horseshoe")
      rbind(data, data.frame(sum_obj$estimates,
        model = rep("Horseshoe", nrow(sum_obj$estimates))
      ))
    } else {
      data
    }
  }
  data$subgroup <- as.factor(data$subgroup)
  data$model <- as.factor(data$model)
  result <- list(
    data = data,
    overall_trt = overall_trt,
    resptype = list_objects[[1]]$resptype
  )
  class(result) <- "compare.data"
  return(result)
}
