#' Data Preprocessing
#'
#' Function to preprocess the data and obtain the design matrices corresponding
#' to a model with just interaction of the covariates with the treatment,
#' to a model with just the main effects and to a model just with the covariates
#' and one-hot encoding.
#'
#' @param trt (`string`)\cr the treatment variable name. The treatment variable
#' must be a factor with 2 levels where the first level is the control and the
#' second one the treatment.
#' @param subgr (`character`)\cr vector with the name of the subgroup variables
#'  from which we want to obtain the subgroup treatment effect.
#' @param covars (`character`)\cr vector with the name of the variables that
#'   we want to include in the model. The `subgr` variables have to be included
#'   here.
#' @param data (`data frame`)\cr the data frame with the variables.
#'
#' @return List with `design_ia`, `design_main`, `design_dummy`, `subgr_names`.
#' @export
#'
#' @examples
#' preprocess("arm", c("x_1", "x_2"), c("x_1", "x_2", "x_3"), example_data)
preprocess <- function(trt, subgr, covars, data) {
  assert_string(trt)
  assert_character(subgr)
  assert_character(covars)
  assert_data_frame(data)
  levels(data[[trt]]) <- c("0", "1")
  covariates <- data[c(trt, covars)]
  subgr_model <- stats::as.formula(paste(
    "~ -1 +", paste(trt, "+"),
    paste0(covars, collapse = "+")
  ))
  design_main <- stats::model.matrix(subgr_model, data = covariates)
  colnames(design_main) <- gsub(" ", "", colnames(design_main))
  colnames(design_main) <- gsub("-", "", colnames(design_main))
  design_ia <- NULL
  design_dummy <- NULL
  for (j in covars) {
    ib_j <- stats::model.matrix(stats::as.formula(paste("~", j, "-1")),
      data = covariates
    )
    design_dummy <- cbind(design_dummy, ib_j)
    ia_j <- ib_j * as.numeric(covariates[[trt]] == "1")
    design_ia <- cbind(design_ia, ia_j)
  }
  colnames(design_dummy) <- gsub(" ", "", colnames(design_dummy))
  colnames(design_dummy) <- gsub("-", "", colnames(design_dummy))
  colnames(design_ia) <- paste(colnames(design_dummy), trt, sep = "_")
  subgr_names <- NULL
  for (k in subgr) {
    ic_k <- stats::model.matrix(stats::as.formula(paste("~", k, "-1")),
      data = covariates
    )
    subgr_names <- c(subgr_names, colnames(ic_k))
  }
  subgr_names <- gsub(" ", "", subgr_names)
  subgr_names <- gsub("-", "", subgr_names)

  list(
    design_ia = design_ia,
    design_main = design_main,
    design_dummy = design_dummy,
    subgr_names = subgr_names
  )
}
