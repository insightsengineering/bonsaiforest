# Main script to run the simulations.
library(checkmate)
library(bonsaiforest)
library(parallel)
library(parallelly)
library(subtee) # Install from https://cran.r-project.org/src/contrib/Archive/subtee/subtee_1.0.1.tar.gz
library(cmdstanr) # Install as described at https://mc-stan.org/cmdstanr/articles/cmdstanr.html
library(dplyr)

# Load scenarios ----

scenario_files <- grep(
  pattern = "scenario\\d\\.rds$",
  x = dir("scenarios", full.names = TRUE),
  value = TRUE
)
assert_character(scenario_files, min.len = 1L)
scenarios <- lapply(scenario_files, readRDS)
str(scenarios, 1)
# List of 6
# $ :List of 1000
# $ :List of 1000
# $ :List of 1000
# $ :List of 1000
# $ :List of 1000
# $ :List of 1000

# Additional functions ----

source("functions.R")

# Run analyses ----

source("population.R") # fast.
source("subgroup.R") # fast.
source("subtee.R") # takes a few minutes.
source("horseshoe.R") # this takes the longest time: several days.
source("ridge.R") # takes a few minutes.
source("lasso.R") # takes a few minutes.
source("truth.R") # takes about an hour.

# Combine analysis results and scenario properties ----

results <- rbind(
  population_results,
  subgroup_results,
  subtee_results,
  ridge_results,
  lasso_results,
  horseshoe_results
) |>
  full_join(scenario_properties, by = c("scenario_no", "subgroup"))
head(results)
