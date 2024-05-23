#' Average Hazard Ratio Estimation
#'
#' Function to obtain the average hazard ratio.
#'
#' @param x_subg (`matrix`)\cr the matrix with the subgroup covariates.
#' @param dummy_subg (`matrix`)\cr the dummy matrix with the subgroup
#'  covariates.
#' @param est_coef (`matrix`)\cr the estimated coefficients from the fitted
#'  model.
#' @param h0 (`numeric`)\cr the vector with the cumulative baseline hazard.
#' @param gamma (`scalar`)\cr numeric value defining the weights to obtain
#' the average hazard ratio. Default is 1 (in this case the average hazard
#' ratio obtained can be interpreted as the odds of concordance).
#'
#' @return Average hazard ratio
#' @export
#'
#' @examples
#' ahr_estimation(
#'   elastic_net_surv$design1, elastic_net_surv$dummy1,
#'   elastic_net_surv$est_coef, elastic_net_surv$h0
#' )
ahr_estimation <- function(x_subg, dummy_subg, est_coef, h0, gamma = 1) {
  assert_matrix(x_subg)
  assert_matrix(dummy_subg)
  assert_matrix(est_coef)
  assert_numeric(h0)
  assert_scalar(gamma)
  res <- list()
  for (i in 0:1) {
    x <- cbind(
      rep(abs(i - 1), nrow(x_subg)), rep(i, nrow(x_subg)),
      x_subg[, 3:(ncol(x_subg) - ncol(dummy_subg))], i * dummy_subg
    )
    res[[i + 1]] <- survival_curves(x, h0, est_coef)
  }
  n <- nrow(res[[1]])
  frac <- list()
  for (i in 1:2) {
    j <- setdiff(c(1, 2), i)
    frac[[i]] <- apply(
      res[[i]]^gamma *
        (res[[j]]^gamma - rbind(1, as.data.frame(res[[j]][-n, ]))^gamma),
      2, sum
    )
  }
  ahr <- as.numeric(frac[[1]] / frac[[2]])
  ahr
}


#' Helper Function to get Kaplan-Meier Estimate
#'
#' @inheritParams naive
#' @param data (`data.frame`)\cr data to use.
#' @param times (`numeric`)\cr time points to use.
#'
#' @return Numeric vector with Kaplan-Meier estimates at `times`.
#' @keywords internal
km_fun <- function(resp, status, data, times) {
  assert_string(resp)
  assert_string(status)

  form <- stats::as.formula(paste("survival::Surv(", resp, ",", status, ") ~ 1"))
  surv_mod <- survival::survfit(form, data = data)
  surv_sum <- summary(surv_mod, times = times, extend = TRUE)
  surv_sum$surv
}

#' Average Hazard Estimation based on Kaplan-Meier Estimates
#'
#' @inheritParams naive
#' @param status (`string`)\cr the status variable name in survival data.
#' @param t_quantile (`number`)\cr definition of the quantile.
#'
#' @returns The estimated average hazard ratio of the treatment (second level of `trt`)
#'   versus the control (first level of `trt`).
#'
#' @details
#' Estimates can be unstable due to the variability of the Kaplan-Meier estimates
#' in the tails. The `t_quantile` argument can address this, e.g. by setting it to
#' 0.95, the 5% highest times will be discarded. By default, the 1% highest times
#' are discarded.
#'
#' @export
#' @examples
#' ahr_from_km("tt_pfs", "arm", example_data, "ev_pfs")
#' ahr_from_km("tt_pfs", "arm", example_data, "ev_pfs", t_quantile = 0.95)
ahr_from_km <- function(resp,
                        trt,
                        data,
                        status,
                        t_quantile = 0.99) {
  assert_string(resp)
  assert_string(trt)
  assert_data_frame(data)
  assert_string(status)
  assert_subset(c(resp, trt, status), names(data))
  assert_factor(data[[trt]], n.levels = 2L)
  assert_numeric(data[[status]])
  assert_true(all(data[[status]] %in% c(0, 1)))
  assert_numeric(data[[resp]], lower = 0, finite = TRUE)
  assert_number(t_quantile, lower = 0.5, upper = 1)

  times_observed <- data[[resp]][data[[status]] == 1]
  time_max <- stats::quantile(times_observed, probs = t_quantile)
  times_unique <- sort(unique(times_observed))
  times <- times_unique[times_unique <= time_max]

  # Calculate the Kaplan-Meier estimate at these times in both groups.
  data_split <- split(data, data[[trt]])
  km_c <- km_fun(resp, status, data_split[[1L]], times)
  km_t <- km_fun(resp, status, data_split[[2L]], times)

  # From this derive the average hazard ratio.
  ahr_num <- sum(km_c * diff(c(1, km_t)))
  ahr_denom <- sum(km_t * diff(c(1, km_c)))
  ahr_num / ahr_denom
}
