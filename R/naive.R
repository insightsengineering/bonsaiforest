#' Naive Model Estimation
#'
#' Function to fit the naive models to the data of each one of the subgroups.
#'
#' @param resp (`string`)\cr the response variable name.
#' @param trt (`string`)\cr the treatment variable name. The treatment variable
#' must be a factor with 2 levels where the first level is the control and the
#' second one the treatment.
#' @param subgr (`character`)\cr vector with the name of the subgrouping
#' variables. These variables have to be `factor` variables with the subgroups
#' as levels.
#' @param data (`data frame`)\cr the data frame with the variables.
#' @param resptype (`string`)\cr the type of data used. Can be "survival"
#'   or "binary".
#' @param status (`string`)\cr only for "survival" `resptype`,
#'   the status variable name in survival data.
#'
#' @return List with `fit`, `estimates`, `model`, `resptype`, `data`.
#' @export
#'
#' @examples
#' naive("tt_pfs", "arm", c("x_1", "x_2"), example_data, "survival", "ev_pfs")
naive <- function(resp, trt, subgr, data,
                  resptype = c("survival", "binary"), status = NULL) {
  assert_string(resp)
  assert_string(trt)
  assert_character(subgr)
  assert_data_frame(data)
  assert_factor(data[[trt]])
  resptype <- match.arg(resptype)
  subgr_model <- stats::as.formula(paste("~", paste0(subgr, collapse = "+")))
  if (resptype == "survival") {
    assert_string(status)
    base_model <- stats::as.formula(paste(
      "Surv(", resp, ",", status, ") ~ ",
      trt
    ))
    stacked_data <- generate_stacked_data(base_model, subgr_model, data,
      resptype = resptype
    )
    list_subg <- split(stacked_data, ~subgroup)
    fit <- lapply(list_subg, FUN = function(data) {
      survival::coxph(survival::Surv(time, status) ~ arm, data = data)
    })
    names(fit) <- gsub("\\.", "", names(fit))
    naive_estimates <- cbind(
      subgroup = names(fit),
      do.call(rbind.data.frame, lapply(fit, broom::tidy))
    )
  } else if (resptype == "binary") {
    base_model <- stats::as.formula(paste(resp, " ~ ", trt))
    stacked_data <- generate_stacked_data(base_model, subgr_model, data,
      resptype = resptype
    )
    list_subg <- split(stacked_data, ~subgroup)
    fit <- lapply(list_subg, FUN = function(data) {
      stats::glm(y ~ arm, data = data, family = "binomial")
    })
    names(fit) <- gsub("\\.", "", names(fit))
    naive_estimates <- cbind(
      subgroup = names(fit),
      do.call(
        rbind.data.frame,
        lapply(fit, broom::tidy)
      )[seq(
        2,
        2 * length(fit),
        2
      ), ]
    )
  }
  result <- list(
    fit = fit,
    estimates = naive_estimates,
    model = "naive",
    resptype = resptype,
    data = data
  )
  class(result) <- c("bonsaiforest", "naive")
  return(result)
}
