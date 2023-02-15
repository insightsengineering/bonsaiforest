#' Print Function for Naivepop Summary
#'
#' Function that prints the summary of a naivepop object
#'
#' @param x (`summary.naivepop`)\cr Summary of a `naivepop` object.
#' @param ... Other arguments of print.
#'
#' @return The summary of the object.
#' @export
#'
#' @examples
#' print(summary(naivepop_fit_bin))
print.summary.naivepop <- function(x, ...) {
  assert_class(x, "summary.naivepop")
  print(x$estimates)
  invisible(x)
}

#' Print Function for Naive Summary
#'
#' Function that prints the summary of a naive object
#'
#' @param x (`summary.naive`)\cr Summary of a `naive` object.
#' @param ... Other arguments of print.
#'
#' @return The summary of the object.
#' @export
#'
#' @examples
#' print(summary(naive_fit_bin))
print.summary.naive <- function(x, ...) {
  assert_class(x, "summary.naive")
  print(x$estimates)
  invisible(x)
}


#' Print Function for Elastic Net Summary
#'
#' Function that prints the summary of a elastic_net object
#'
#' @param x (`summary.elastic_net`)\cr Summary of a `elastic_net` object.
#' @param ... Other arguments of print.
#'
#' @return The summary of the object.
#' @export
#'
#' @examples
#' print(summary(elastic_net_fit_bin))
print.summary.elastic_net <- function(x, ...) {
  assert_class(x, "summary.elastic_net")
  print(x$estimates)
  invisible(x)
}


#' Print Function for Horseshoe Summary
#'
#' Function that prints the summary of a horseshoe object
#'
#' @param x (`summary.horseshoe`)\cr Summary of a `horseshoe` object.
#' @param ... Other arguments of print.
#'
#' @return The summary of the object.
#' @export
#'
#' @examples
#' print(summary(horseshoe_fit_bin))
print.summary.horseshoe <- function(x, ...) {
  assert_class(x, "summary.horseshoe")
  print(x$estimates)
  invisible(x)
}
