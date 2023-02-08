#' H0, Coefficients Elastic Net Survival Model and Matrices
#'
#' List with the cumulative baseline hazard, with the estimated coefficients
#' of the model fitted with an elastic net and survival data, with the general
#' design matrix and with the design and dummy matrices for subgroup x_1a.
#'
#'
#' @format
#' A `list` with three elements:
#'   - `h0`: the cumulative baseline hazard.
#'   - `est_coef`: the estimated coefficients of the fitted model.
#'   - `x`: the design matrix of the model.
#'   - `dummy1`: the dummy matrix of the model for subgroup x_1a.
#'   - `design1`: the design matrix of the model for subgroup x_1a.
#' @source This is an artificial example.
"elastic_net_surv"
