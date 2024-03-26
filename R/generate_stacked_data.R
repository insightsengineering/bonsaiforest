#' Generation of Stacked Data by Subgroups
#'
#' Function to generate stacked data by the subgroups considered in the model.
#'
#' @param base_model (`formula`)\cr the formula of the basic model that is going
#' to be fitted. Depending on `resptype` it will be the formula corresponding
#' to a coxph model or to a glm model. In the case of "survival" a formula like
#' Surv(time, status) ~ trt would be expected  and in the "binary" case a
#' formula like y ~ trt.
#' @param subgroup_model (`formula`)\cr the formula with all the subgroup
#' variables.
#' @param data (`data frame`)\cr the data frame with the variables.
#' @param resptype (`string`)\cr the type of data used. Can be "survival"
#' or "binary".
#'
#' @return Data frame of the stacked data.
#' @export
#'
#' @examples
#' generate_stacked_data(Surv(tt_pfs, ev_pfs) ~ arm, ~ x_1 + x_2, example_data, "survival")
generate_stacked_data <- function(base_model, subgroup_model, data,
                                  resptype = c("survival", "binary")) {
  assert_formula(base_model)
  assert_formula(subgroup_model)
  assert_data_frame(data)
  resptype <- match.arg(resptype)

  data <- tibble::as_tibble(data)
  subgroup_vars <- all.vars(subgroup_model)
  resp_var <- all.vars(base_model)[1]
  if (resptype == "survival") {
    status_var <- all.vars(base_model)[2]
    arm_var <- all.vars(base_model)[3]
  } else if (resptype == "binary") {
    status_var <- NULL
    arm_var <- all.vars(base_model)[2]
  }
  tmp <- lapply(data[, subgroup_vars], function(x) levels(factor(x)))
  subgroup_names <- paste(rep(names(tmp), lapply(tmp, length)),
    unlist(tmp),
    sep = "."
  )
  data <- data[, c(arm_var, resp_var, status_var, subgroup_vars)]
  data[, subgroup_vars] <- lapply(data[, subgroup_vars], as.character)
  drop_vars <- c(arm_var, resp_var, status_var)
  d <- tidyr::pivot_longer(
    data,
    names_to = "subgroup_var",
    values_to = "subgroup_value",
    -tidyselect::any_of(drop_vars)
  )
  subgroup <- paste(d$subgroup_var, d$subgroup_value, sep = ".")
  subgroup <- factor(subgroup, levels = subgroup_names)
  d <- cbind(d, subgroup)
  d <- dplyr::arrange(d, subgroup)
  d <- if (resptype == "survival") {
    dplyr::rename_with(
      d,
      ~ c("arm", "time", "status"),
      tidyselect::all_of(c(arm_var, resp_var, status_var))
    )
  } else if (resptype == "binary") {
    dplyr::rename_with(
      d,
      ~ c("arm", "y"),
      tidyselect::all_of(c(arm_var, resp_var))
    )
  }
  d
}
