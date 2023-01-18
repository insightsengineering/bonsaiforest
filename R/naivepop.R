#' Naivepop function
#'
#' Function to fit the naivepop model to the data. This model is going to
#'estimate the subgroup treatment effects with the overall population
#'treatment effect.
#'
#' @param resp (`character`) \cr for the response variable name.
#' @param trt (`character`) \cr for the treatment variable name.
#' @param data (`data frame`) \cr for the data frame with the variables.
#' @param resptype (`character`) \cr for the type of data used. Can be "survival"
#' or "binary".
#' @param status (`character`) \cr for the status variable name in survival data.
#'
#' @return List with fit, model, resptype, data
#' @export
#'
#' @examples
#' naivepop("futime", "trt", survival::myeloid, "survival", "death")
naivepop <- function(resp, trt, data, resptype, status = NULL){
  assert_character(resp, len = 1)
  assert_character(trt, len = 1)
  assert_data_frame(data)
  assert_character(resptype, len = 1)
  assert_character(status, len =1, null.ok = TRUE)

  if (resptype == "survival"){
    form_surv <- as.formula(paste("Surv(", resp, ",", status, ") ~ ", trt))
    fit_pop <- coxph(formula = form_surv, data = data)
  } else if (resptype == "binary"){
    form_bin <- as.formula(paste(resp, " ~ ", trt))
    fit_pop <- glm(formula = form_bin, data = data, family = "binomial")
  }

  list(fit = fit_pop, model = "naive_pop", resptype = resptype, data = data)
}
