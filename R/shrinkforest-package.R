#' @keywords internal
"_PACKAGE"

#' @useDynLib shrinkforest, .registration = TRUE

#' @import checkmate
#' @importFrom survival aareg
#' @importFrom stats acf
#' @importFrom tibble as_tibble
#' @importFrom tidyr gather
#' @importFrom dplyr arrange
#' @importFrom broom tidy
#' @importFrom glmnet cv.glmnet
#' @importFrom brms brm
#' @importFrom splines2 mSpline
#' @importFrom Rcpp evalCpp
#' @importFrom gbm gbm
NULL
