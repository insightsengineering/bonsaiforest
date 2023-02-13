#' Print Function for Horseshoe Summary
#'
#' Function that prints the summary of a horseshoe object
#'
#' @param x (`summary.horseshoe`)\cr Summary of a `horseshoe` object.
#' @param ... Other arguments of print
#'
#' @return The summary of the object
#' @export
#'
#' @examples
#' print(summary(horseshoe_fit_bin))
print.summary.horseshoe <- function(x, ...) {
  assert_class(x, "summary.horseshoe")
  print(x$summary_post)
  invisible(x)
}
