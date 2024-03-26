#' Elastic Net Penalization Model Estimation
#'
#' Function to fit the elastic net penalization model to the data. This model
#' penalizes the interaction between the covariates and the treatment but
#' leaves unpenalized the main effects.
#'
#' @param resp (`string`)\cr the response variable name.
#' @param trt (`string`)\cr the treatment variable name. The treatment variable
#' must be a factor with 2 levels where the first level is the control and the
#' second one the treatment.
#' @param subgr (`character`)\cr vector with the name of the subgroup variables
#' from which we want to obtain the subgroup treatment effect. They have to be
#' `factor` variables with the subgroups as levels.
#' @param covars (`character`)\cr vector with the name of the variables that
#' we want to include in the model. They have to be `factor` variables with the
#' subgroups as levels. The `subgr` variables have to be included here.
#' @param data (`data frame`)\cr the data frame with the variables.
#' @param resptype (`string`)\cr the type of data used. Can be "survival"
#' or "binary".
#' @param alpha (`scalar`)\cr the elastic net mixing parameter with values
#' between 0 and 1. The special case of `alpha`=1 corresponds to a lasso
#' penalty and the case of `alpha`=0 to a ridge penalty.
#' @param status (`string`)\cr only for "survival" `resptype`,
#' the status variable name in survival data.
#'
#' @return List with `fit`, `model`, `resptype`, `data`, `alpha`,
#'  `design_matrix`, `design_dummy`, `y`, `subgr_names`.
#' @export
#'
#' @examples
#' elastic_net(
#'   "tt_pfs", "arm", c("x_1", "x_2"), c("x_1", "x_2", "x_3"),
#'   example_data, "survival", 1, "ev_pfs"
#' )
elastic_net <- function(resp, trt, subgr, covars, data,
                        resptype = c("survival", "binary"), alpha,
                        status = NULL) {
  assert_string(resp)
  assert_string(trt)
  assert_character(subgr)
  assert_character(covars)
  assert_data_frame(data)
  assert_scalar(alpha)
  assert_factor(data[[trt]])
  resptype <- match.arg(resptype)
  prep_data <- preprocess(trt, subgr, covars, data)
  if (resptype == "survival") {
    assert_string(status)
    penalty_factor <- c(
      rep(0, ncol(prep_data$design_main)),
      rep(1, ncol(prep_data$design_ia))
    )
    design_matrix <- cbind(prep_data$design_main, prep_data$design_ia)
    fit_glmnet <- glmnet::cv.glmnet(design_matrix,
      survival::Surv(data[[resp]], data[[status]]),
      family = "cox",
      penalty.factor = penalty_factor,
      alpha = alpha
    )
    y <- as.data.frame(cbind(data[[resp]], data[[status]]))
    colnames(y) <- c("resp", "status")
  } else if (resptype == "binary") {
    penalty_factor <- c(
      rep(0, ncol(prep_data$design_main[, -1])),
      rep(1, ncol(prep_data$design_ia))
    )
    design_matrix <- cbind(prep_data$design_main[, -1], prep_data$design_ia)
    fit_glmnet <- glmnet::cv.glmnet(design_matrix, data[[resp]],
      family = "binomial",
      penalty.factor = penalty_factor,
      alpha = alpha
    )
    y <- data[[resp]]
  }
  result <- list(
    fit = fit_glmnet,
    model = "elastic_net",
    resptype = resptype,
    data = data,
    alpha = alpha,
    design_matrix = design_matrix,
    design_dummy = prep_data$design_dummy,
    y = y,
    subgr_names = prep_data$subgr_names
  )
  class(result) <- c("bonsaiforest", "elastic_net")
  return(result)
}
