#' @keywords internal
"_PACKAGE"

#' @useDynLib bonsaiforest, .registration = TRUE

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
#' @import ggplot2
#' @importFrom forcats fct_inorder
#' @importFrom vdiffr expect_doppelganger
NULL
