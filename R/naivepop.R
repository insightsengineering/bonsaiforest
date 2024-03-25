#' Naive Overall Population Model Estimation
#'
#' Function to fit the naive overall population model to the data. This model is going to
#' estimate the subgroup treatment effects with the overall population
#' treatment effect.
#'
#' @param resp (`string`)\cr the response variable name.
#' @param trt (`string`)\cr the treatment variable name. The treatment variable
#' must be a factor with 2 levels where the first level is the control and the
#' second one the treatment.
#' @param data (`data frame`)\cr the data frame with the variables.
#' @param resptype (`string`)\cr the type of data used. Can be "survival"
#'   or "binary".
#' @param status (`string`)\cr only for "survival" `resptype`,
#'   the status variable name in survival data.
#'
#' @return List with `fit`, `model`, `resptype`, `data`.
#' @export
#'
#' @examples
#' naivepop("tt_pfs", "arm", example_data, "survival", "ev_pfs")
naivepop <- function(resp, trt, data, resptype = c("survival", "binary"), status = NULL) {
  assert_string(resp)
  assert_string(trt)
  assert_data_frame(data)
  assert_factor(data[[trt]])
  resptype <- match.arg(resptype)

  fit_pop <- if (resptype == "survival") {
    assert_string(status)
    form_surv <- stats::as.formula(paste("survival::Surv(", resp, ",", status, ") ~ ", trt))
    survival::coxph(formula = form_surv, data = data)
  } else if (resptype == "binary") {
    form_bin <- stats::as.formula(paste(resp, " ~ ", trt))
    stats::glm(formula = form_bin, data = data, family = "binomial")
  }

  result <- list(
    fit = fit_pop,
    model = "naive_pop",
    resptype = resptype,
    data = data
  )
  class(result) <- c("bonsaiforest", "naivepop")
  return(result)
}
