#' Compare Forest Plots
#'
#' Function to obtain a forest plot with all the different fitted models in
#' order to compare their performance.
#'
#' @param ... Summary of `shrinkforest` objects separated by commas. There
#' should be at least two objects of this kind in the call of the function.
#'
#' @return Forest plot with all the fitted models.
#' @export
#'
#' @examples
#' compareplot(
#'   summary(naive_fit_surv), summary(naivepop_fit_surv),
#'   summary(elastic_net_fit_surv)
#' )
compareplot <- function(...) {
  list_objects <- list(...)
  data <- NULL
  overall_trt <- NULL
  for (obj in list_objects) {
    if (inherits(obj, "summary.naivepop")) {
      assert_class(obj, "summary.naivepop")
      overall_trt <- obj$estimates
    } else if (inherits(obj, "summary.naive")) {
      assert_class(obj, "summary.naive")
      data_obj <- data.frame(obj$estimates,
        model = rep("Naive", nrow(obj$estimates))
      )
      data <- rbind(data, data_obj)
    } else if (inherits(obj, "summary.elastic_net")) {
      assert_class(obj, "summary.elastic_net")
      data_obj <- data.frame(obj$estimates,
        trt.low = obj$estimates[, 2],
        trt.high = obj$estimates[, 2],
        model = rep(
          paste("Elastic net alpha =", obj$alpha),
          nrow(obj$estimates)
        )
      )
      data <- rbind(data, data_obj)
    } else if (inherits(obj, "summary.horseshoe")) {
      assert_class(obj, "summary.horseshoe")
      data_obj <- data.frame(obj$estimates,
        model = rep("Horseshoe", nrow(obj$estimates))
      )
      data <- rbind(data, data_obj)
    }
  }
  data$subgroup <- as.factor(data$subgroup)
  data$model <- as.factor(data$model)
  resptype <- list_objects[[1]]$resptype
  forestplot <- ggplot(
    data = data,
    aes(
      x = trt.estimate, y = forcats::fct_inorder(model), xmin = trt.low,
      xmax = trt.high
    )
  ) +
    ggtitle("Forest plot") +
    geom_pointrange(aes(col = forcats::fct_inorder(model))) +
    ylab("Subgroup") +
    geom_errorbar(aes(
      xmin = trt.low, xmax = trt.high,
      col = forcats::fct_inorder(model)
    ), width = 0.5, cex = 1) +
    facet_wrap(~ forcats::fct_inorder(subgroup),
      strip.position = "left",
      nrow = nrow(data), scales = "free_y"
    ) +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.x = element_text(face = "bold"),
      axis.title = element_text(size = 12, face = "bold"),
      strip.text.x = element_text(hjust = 0, vjust = 1, angle = 180, face = "bold"),
      strip.text.y.left = element_text(angle = 0)
    ) +
    scale_colour_discrete("Subgroup-specific")
  if (resptype == "survival") {
    forestplot2 <- forestplot + xlab("Hazard ratio")
  } else if (resptype == "binary") {
    forestplot2 <- forestplot + xlab("Log Odds-Ratio")
  }
  if (!is.null(overall_trt)) {
    forestplot2 + geom_vline(aes(xintercept = overall_trt, linetype = "Overall"),
      color = "darkblue"
    ) +
      scale_linetype_manual("Overall population", values = c("dashed"))
  } else {
    forestplot2
  }
}
