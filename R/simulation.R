#' Helper for Cutting into Normal Quantiles
#'
#' @param x (`numeric`)\cr continuous covariate values.
#' @param prob (`numeric`)\cr probabilities for the standard normal quantiles.
#' @param labels (`character`)\cr corresponding labels for the resulting factor.
#'
#' @return The factor variable.
#' @keywords internal
cut_norm_quant <- function(x, prob, labels = letters[seq_along(c(prob, 1))]) {
  assert_numeric(x)
  assert_numeric(prob)
  assert_character(labels)
  assert_true(identical(length(prob) + 1L, length(labels)))

  norm_quantiles <- stats::qnorm(p = prob)
  breaks <- c(-Inf, norm_quantiles, Inf)
  cut(x, breaks, labels = labels)
}

#' Generation of a Design Matrix for Simulations
#'
#' This function uses a block diagonal covariance matrix for the underlying
#' multivariate normal data to create the design matrix in blocks of 10, see
#' the details.
#'
#' @details
#' The following pattern is repeated for the covariate blocks:
#'
#' - The first 5 covariates are uncorrelated with everything.
#' - The covariates 6 to 8 have "moderate" correlation (0.25) between each other.
#' - The covariates 9 and 10 have "high" correlation (0.5).
#'
#' By default, only the resulting categorical covariates obtained by thresholding
#' are included. Optionally also the original continuous covariates are included
#' in the returned design matrix.
#'
#' @param n (`count`)\cr number of rows (observations).
#' @param p_catvar (`count`)\cr number of covariates (excluding treatment arm).
#' @param add_contvars (`flag`)\cr whether to add continuous covariates.
#' @param arm_factor (`flag`)\cr whether to make the arm variable a factor.
#'
#' @return The design matrix.
#' @export
#'
#' @examples
#' simul_covariates(n = 10, p_catvar = 3, add_contvars = FALSE)
#' simul_covariates(n = 10, p_catvar = 3, add_contvars = TRUE)
#' simul_covariates(n = 10, p_catvar = 3, add_contvars = TRUE, arm_factor = TRUE)
simul_covariates <- function(n, p_catvar = 10, add_contvars = FALSE, arm_factor = FALSE) {
  assert_count(n, positive = TRUE)
  assert_count(p_catvar, positive = TRUE)
  assert_flag(add_contvars)
  assert_flag(arm_factor)

  sigma <- matrix(0, nrow = 10, ncol = 10)
  first_grp <- 1:5
  second_grp <- 6:8
  third_grp <- 9:10
  sigma[first_grp, first_grp] <- 0
  sigma[second_grp, second_grp] <- 0.25
  sigma[third_grp, third_grp] <- 0.5
  diag(sigma) <- 1

  no_10_blocks <- ceiling(p_catvar / 10)
  x <- NULL
  z <- NULL
  for (j in seq_len(no_10_blocks)) {
    # Continuous version.
    z_j <- data.frame(MASS::mvrnorm(n, mu = rep(0, 10), Sigma = sigma))
    colnames(z_j) <- paste("z", (j - 1) * 10 + 1:10, sep = "_")
    z <- if (j == 1) {
      z_j
    } else {
      cbind(z, z_j)
    }
    # Categorized version.
    x_j <- data.frame(
      v1 = cut_norm_quant(z_j[, 1], prob = 0.5),
      v2 = cut_norm_quant(z_j[, 2], prob = 0.4),
      v3 = cut_norm_quant(z_j[, 3], prob = 0.2),
      v4 = cut_norm_quant(z_j[, 4], prob = c(0.3, 0.6)),
      v5 = cut_norm_quant(z_j[, 5], prob = c(0.15, 0.3, 0.6)),
      v6 = cut_norm_quant(z_j[, 6], prob = 0.4),
      v7 = cut_norm_quant(z_j[, 7], prob = 0.4),
      v8 = cut_norm_quant(z_j[, 8], prob = c(0.2, 0.5)),
      v9 = cut_norm_quant(z_j[, 9], prob = 0.2),
      v10 = cut_norm_quant(z_j[, 10], prob = c(0.2, 0.5))
    )
    colnames(x_j) <- paste("x", (j - 1) * 10 + seq_len(10), sep = "_")
    x <- if (j == 1) {
      x_j
    } else {
      cbind(x, x_j)
    }
  }
  n_ctrl <- n %/% 2
  n_exp <- n - n_ctrl
  trt_arm <- sample(
    rep(
      c(0, 1),
      c(n_ctrl, n_exp)
    )
  )
  index_catvar <- seq_len(p_catvar)
  x <- cbind(
    arm = if (arm_factor) factor(trt_arm) else trt_arm,
    x[, index_catvar, drop = FALSE]
  )
  if (add_contvars) {
    x <- cbind(x, z[, index_catvar, drop = FALSE])
  }
  x
}

#' Simulation of Progression Free Survival Times
#'
#' @param lp_aft (`numeric`)\cr linear predictor values for the accelerate failure time model (AFT).
#' @param sigma_aft (`number`)\cr standard deviation for the AFT model.
#' @param recr_duration (`number`)\cr duration of recruitment.
#' @param rate_cens (`number`)\cr rate for the exponentially distributed censoring process.
#' @param n_events (`count`)\cr number of events to reach for the study end.
#' @param add_uncensored_pfs (`flag`)\cr whether to add the uncensored PFS as well to the resulting
#'   `data.frame`.
#'
#' @return A `data.frame` with columns `tt_pfs` (PFS time) and `ev_pfs` (corresponding
#'   event indicator with 1 for an event and 0 for censored), and optionally
#'   `tt_pfs_uncens`.
#' @export
#'
#' @examples
#' set.seed(123)
#' simul_pfs(
#'   lp_aft = rnorm(100),
#'   sigma_aft = 1,
#'   recr_duration = 0.2,
#'   rate_cens = 2,
#'   n_events = 20
#' )
simul_pfs <- function(lp_aft,
                      sigma_aft,
                      recr_duration,
                      rate_cens,
                      n_events,
                      add_uncensored_pfs = FALSE) {
  assert_numeric(lp_aft)
  assert_number(sigma_aft, lower = .Machine$double.xmin)
  assert_number(recr_duration, lower = .Machine$double.xmin)
  assert_number(rate_cens, lower = .Machine$double.xmin)
  assert_count(n_events, positive = TRUE)
  assert_flag(add_uncensored_pfs)

  n <- length(lp_aft)
  # Uncensored event time.
  log_tt_pfs <- c(lp_aft + sigma_aft * log(stats::rexp(n, rate = 1)))
  tt_pfs_uncens <- exp(log_tt_pfs)

  # Censoring step 1:
  # with rate_cens.
  tt_pfs_cens1 <- stats::rexp(n, rate = rate_cens)
  tt_pfs_cens1 <- pmin(tt_pfs_uncens, tt_pfs_cens1)
  ev_pfs_cens1 <- ifelse(tt_pfs_uncens <= tt_pfs_cens1, 1, 0)
  if (sum(ev_pfs_cens1) < n_events) {
    stop(paste(
      "Impossible to reach", n_events,
      "events with", n, "patients,",
      "a censoring rate of", rate_cens,
      "and the specified linear predictor."
    ))
  }

  # Censoring step 2:
  # due to staggerred recruitment and recruiting only until target_ev
  # events have been observed.
  rec_time <- stats::runif(n, min = 0, max = recr_duration)
  tt_pfs_cens1_calendar <- rec_time + tt_pfs_cens1
  study_stop_time <- sort(tt_pfs_cens1_calendar[ev_pfs_cens1 == 1])[n_events]
  if (study_stop_time < max(rec_time)) {
    warning("Target number of events reached before all subjects were enrolled.")
  }

  tt_pfs <- pmax(0, pmin(tt_pfs_cens1_calendar, study_stop_time) - rec_time)
  ev_pfs <- ifelse(tt_pfs_cens1_calendar <= study_stop_time, ev_pfs_cens1, 0)
  result <- data.frame(tt_pfs = tt_pfs, ev_pfs = ev_pfs)
  if (add_uncensored_pfs) {
    result$tt_pfs_uncens <- tt_pfs_uncens
  }
  result
}

#' Simulate Covariates and Progression Free Survival Data
#'
#' This combines the covariates simulation via [simul_covariates()] with 10
#' categorical covariates, and the PFS simulation via [simul_pfs()].
#'
#' @details
#' Regression coefficients are for an AFT with over-parametrized dummy
#' coding for arm-subgroup interactions.
#'
#' @param n (`count`)\cr number of patients.
#' @param coefs (`numeric`)\cr named vector of coefficients to set.
#' @param add_interaction (`flag`)\cr whether to add interaction terms between covariates
#'   1 and 2.
#' @param \dots additional parameters apart from the linear predictor values
#'   needed for [simul_pfs()].
#'
#' @return A combined `data.frame` with the `id` column, the design matrix and the
#'    PFS outcomes.
#' @export
#'
#' @examples
#' set.seed(321)
#' simul_data(
#'   n = 100,
#'   coefs = c(arm1 = 1),
#'   sigma_aft = 1,
#'   recr_duration = 0.2,
#'   rate_cens = 2,
#'   n_events = 20
#' )
simul_data <- function(n,
                       add_interaction = FALSE,
                       coefs,
                       ...) {
  assert_flag(add_interaction)
  assert_numeric(coefs, min.len = 1L, names = "unique")
  covariates <- simul_covariates(n = n, p_catvar = 10, add_contvars = FALSE, arm_factor = TRUE)
  subgroup_model <- ~ x_1 + x_2 + x_3 + x_4 + x_5 + x_6 + x_7 + x_8 + x_9 + x_10
  if (add_interaction) {
    covariates$x_1_2 <- factor(
      with(
        covariates,
        paste(as.character(x_1), as.character(x_2), sep = "")
      )
    )
    subgroup_model <- stats::update(subgroup_model, ~ . + x_1_2)
  }
  design_main <- stats::model.matrix(
    stats::update(subgroup_model, ~ arm + .),
    data = covariates
  )
  subgroup_vars <- all.vars(subgroup_model)
  design_ia <- NULL
  for (j in subgroup_vars) {
    ia_j <- stats::model.matrix(
      stats::as.formula(paste("~", j, "-1")),
      data = covariates
    ) * design_main[, "arm1"]
    design_ia <- cbind(design_ia, ia_j)
  }
  colnames(design_ia) <- paste(colnames(design_ia), "arm", sep = "_")
  colnames(design_ia) <- gsub(" ", "", colnames(design_ia))
  design_matrix <- cbind(design_main, design_ia)

  if (add_interaction) {
    # Remove created variable again such that final covariates dataset is identical for all scenarios.
    covariates$x_1_2 <- NULL
  }

  reg_coef <- rep(0, ncol(design_matrix))
  names(reg_coef) <- colnames(design_matrix)
  assert_subset(names(coefs), names(reg_coef))
  reg_coef[names(coefs)] <- coefs

  lp_aft <- design_matrix %*% reg_coef
  outcome <- simul_pfs(lp_aft = lp_aft, ...)
  d <- cbind(id = 1:n, covariates, outcome)
  d
}
