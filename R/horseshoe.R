#' Bayesian Shrinkage Model Estimation
#'
#' Function to fit a bayesian shrinkage model with a regularized horseshoe prior
#' on the interaction coefficients between the subgrouping covariates and
#' the treatment.
#'
#' @param resp (`string`)\cr the response variable name.
#' @param trt (`string`)\cr the treatment variable name.The treatment variable
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
#' @param status (`string`)\cr only for "survival" `resptype`,
#' the status variable name in survival data.
#' @param ... Additional arguments from the `brm` function.
#'
#'
#' @return List with `fit`, `model`, `resptype`, `data`, `alpha`,
#'  `design_matrix`, `design_dummy`, `y`, `subgr_names`.
#' @export
#'
#' @examples
#' \donttest{
#' horseshoe("ev_pfs", "arm", c("x_1", "x_2"), c("x_1", "x_2", "x_3"),
#'   example_data, "binary",
#'   chains = 1, seed = 0, control = list(adapt_delta = 0.95),
#'   iter = 50 # In practice, you need to omit this or set it much higher!
#' )
#' }
horseshoe <- function(resp, trt, subgr, covars, data,
                      resptype = c("survival", "binary"), status = NULL,
                      ...) {
  assert_string(resp)
  assert_string(trt)
  assert_character(subgr)
  assert_character(covars)
  assert_data_frame(data)
  assert_factor(data[[trt]])
  resptype <- match.arg(resptype)
  list_arguments <- list(...)
  prep_data <- preprocess(trt, subgr, covars, data)
  form_b <- stats::as.formula(paste(
    "b ~ 0 +",
    paste0(colnames(prep_data$design_ia),
      collapse = " + "
    )
  ))
  if (resptype == "survival") {
    assert_string(status)
    design_matrix <- cbind(prep_data$design_main, prep_data$design_ia)
    form <- stats::as.formula(paste(resp, "|cens(1 - ", status, ") ~ a + b"))
    form_a <- stats::as.formula(paste(
      "a ~ 0 +",
      paste0(colnames(prep_data$design_main),
        collapse = " + "
      )
    ))
    y <- as.data.frame(cbind(data[[resp]], data[[status]]))
    colnames(y) <- c(resp, status)
    data_model <- cbind(design_matrix, y)
    sort_resp <- sort(y[, 1])
    diff_resp <- min(sort_resp - c(0, sort_resp[-length(y[, 1])]))
    limits_resp <- c(max(min(y[, 1]) - diff_resp, 0), max(y[, 1]) + diff_resp)
    quantiles_resp <- stats::quantile(y[, 1], c(0.25, 0.5, 0.75))
    bhaz <- list(
      Boundary.knots = limits_resp, knots = quantiles_resp,
      intercept = FALSE
    )
    family <- brms::brmsfamily("cox", bhaz = bhaz)
  } else if (resptype == "binary") {
    design_main <- prep_data$design_main[, -1]
    design_matrix <- cbind(design_main, prep_data$design_ia)
    form <- stats::as.formula(paste(resp, " ~ a + b"))
    form_a <- stats::as.formula(
      paste(
        "a ~ 1 +",
        paste0(colnames(design_main), collapse = " + ")
      )
    )
    y <- as.data.frame(data[[resp]])
    colnames(y) <- resp
    data_model <- cbind(design_matrix, y)
    family <- brms::brmsfamily("bernoulli")
  }
  fit_function <- function(...) {
    brms::brm(
      brms::bf(form, nl = TRUE) +
        brms::lf(form_a) +
        brms::lf(form_b),
      data = data_model,
      family = family,
      brms::prior_string("normal(0, 5)", class = "b", nlpar = "a") +
        brms::prior_string("horseshoe(1)", class = "b", nlpar = "b"),
      ...
    )
  }
  fit_brms <- do.call(fit_function, args = list_arguments)
  result <- list(
    fit = fit_brms,
    model = "horseshoe",
    resptype = resptype,
    data = data,
    design_matrix = design_matrix,
    design_dummy = prep_data$design_dummy,
    y = y,
    subgr_names = prep_data$subgr_names
  )
  class(result) <- c("bonsaiforest", "horseshoe")
  return(result)
}
