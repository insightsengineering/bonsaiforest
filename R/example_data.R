#' Example data
#'
#' A simulated data set for a realistic scenario of a clinical trial.
#'
#'
#' @format ## `example_data`
#' A data frame with 1000 rows and 14 columns:
#' \describe{
#'   \item{id}{Integer variable with the id of the patient.}
#'   \item{arm}{Factor variable with 2 levels: control and treatment.}
#'   \item{x_1, x_2, x_3, x_4, x_5, x_6, x_7, x_8, x_9, x_10}{Factor variables
#'   that represent covariates of the patients (e.g. sex, region, race, etc.)}
#'   \item{tt_pfs}{Continuous variable with the progression free survival time
#'   of each patient.}
#'   \item{ev_pfs}{Binary variable with the progression free survival status
#'   of each patient. 0 indicates that the event of interest did not happen
#'   and 1 indicates that it did.}
#'   ...
#' }
"example_data"
