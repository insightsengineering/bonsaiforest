#' Forest plot Summary Naive
#'
#' Plot the forest plot of the summary of a `naive` object.
#'
#' @param x (`summary.naive`)\cr summary of a `naive` object.
#' @param ... Additional arguments to plot
#'
#' @return Forest plot
#' @export
#'
#' @examples
#' plot(summary(naive_fit_surv))
plot.summary.naive <- function(x, ...) {
  assert_class(x, "summary.naive")
  data <- data.frame(x$estimates, model = rep("Naive", nrow(x$estimates)))
  data$subgroup <- as.factor(data$subgroup)
  forestplot <- ggplot(
    data = data,
    aes(x = trt.estimate, y = model, xmin = trt.low, xmax = trt.high)
  ) +
    ggtitle("Forest plot Naive") +
    geom_pointrange(aes(col = model)) +
    ylab("Subgroup") +
    geom_errorbar(aes(xmin = trt.low, xmax = trt.high, col = model), width = 0.5, cex = 1) +
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
    scale_colour_discrete("Model")
  if(x$resptype == "survival") {
    forestplot + xlab("Hazard ratio") +
    geom_vline(xintercept = 1, linetype = 2)
  } else if (x$resptype == "binary") {
    forestplot + xlab("Log Odds-Ratio") +
    geom_vline(xintercept = 0, linetype = 2)
  }
}


#' Forest plot Summary Elastic Net
#'
#' Plot the forest plot of the summary of a `elastic_net` object.
#'
#' @param x (`summary.elastic_net`)\cr summary of a `elastic_net` object.
#' @param ... Additional arguments to plot
#'
#' @return Forest plot
#' @export
#'
#' @examples
#' plot(summary(elastic_net_fit_surv))
plot.summary.elastic_net <- function(x, ...) {
  assert_class(x, "summary.elastic_net")
  if (x$alpha == 1) {
    data <- data.frame(x$estimates, model = rep("Lasso", nrow(x$estimates)))
  } else if (x$alpha == 0) {
    data <- data.frame(x$estimates, model = rep("Ridge", nrow(x$estimates)))
  } else {
    data <- data.frame(x$estimates, model = rep("Elastic net", nrow(x$estimates)))
  }
  data$subgroup <- as.factor(data$subgroup)
  forestplot <- ggplot(
    data = data,
    aes(x = trt.estimate, y = model)
  ) +
    ggtitle("Forest plot Elastic Net") +
    geom_point(aes(col = model)) +
    ylab("Subgroup") +
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
    scale_colour_discrete("Model")
  if (x$resptype == "survival") {
    forestplot + xlab("Hazard ratio") +
      geom_vline(xintercept = 1, linetype = 2)
  } else if (x$resptype == "binary") {
    forestplot + xlab("Log Odds-Ratio") +
      geom_vline(xintercept = 0, linetype = 2)
  }
}


#' Forest plot Summary Horseshoe
#'
#' Plot the forest plot of the summary of a `horseshoe` object.
#'
#' @param x (`summary.horseshoe`)\cr summary of a `horseshoe` object.
#' @param ... Additional arguments to plot
#'
#' @return Forest plot
#' @export
#'
#' @examples
#' plot(summary(horseshoe_fit_bin))
plot.summary.horseshoe <- function(x, ...) {
  assert_class(x, "summary.horseshoe")
  data <- data.frame(x$estimates, model = rep("Horseshoe", nrow(x$estimates)))
  data$subgroup <- as.factor(data$subgroup)
  forestplot <- ggplot(
    data = data,
    aes(x = trt.estimate, y = model, xmin = trt.low, xmax = trt.high)
  ) +
    ggtitle("Forest plot Horseshoe") +
    geom_pointrange(aes(col = model)) +
    ylab("Subgroup") +
    geom_errorbar(aes(xmin = trt.low, xmax = trt.high, col = model), width = 0.5, cex = 1) +
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
    scale_colour_discrete("Model")
  if(x$resptype == "survival") {
    forestplot + xlab("Hazard ratio") +
      geom_vline(xintercept = 1, linetype = 2)
  } else if (x$resptype == "binary") {
    forestplot + xlab("Log Odds-Ratio") +
      geom_vline(xintercept = 0, linetype = 2)
  }
}
