#' Little Test Function doing Stuff
#'
#' This is testing documenting arguments. Because it is cool.
#'
#' @param bla (`string`)\cr for bla stuff.
#' @param bli (`numeric`)\cr for bli stuff.
#' @param blu (`list`)\cr for blu.
#' @param ... additional arguments.
#'
#' @return List with bla, bli, blu.
#' @export
#'
#' @examples
#' test("bli", 2, list(1, 4, 5))
test <- function(bla,
                 bli,
                 blu,
                 ...) {
  assert_string(bla)
  assert_numeric(bli)
  assert_list(blu)

  list(bla, bli, blu, ...)
}
