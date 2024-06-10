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

# We keep the scenario 2_20 separate because only horseshoe_2_50 will use it:
scenario_2_20 <- list(readRDS(file.path("scenarios", "scenario2_20.rds")))

# Additional functions ----

source("functions.R")

# Run analyses ----

all_subgroups <- c(
  "S_1a", "S_1b", "S_2a", "S_2b", "S_3a", "S_3b", "S_4a", "S_4b",
  "S_4c", "S_5a", "S_5b", "S_5c", "S_5d", "S_6a", "S_6b", "S_7a",
  "S_7b", "S_8a", "S_8b", "S_8c", "S_9a", "S_9b", "S_10a", "S_10b",
  "S_10c"
)

source("population.R") # fast.
source("subgroup.R") # fast.
source("subtee.R") # takes a few minutes.
source("horseshoe.R") # this takes the longest time: several days.
source("ridge.R") # takes a few minutes.
source("lasso.R") # takes a few minutes.
source("truth.R") # takes about an hour.

## Additional analyses as described in section 3.5 ----
source("horseshoe_2_3.R")
source("horseshoe_2_9.R")
source("horseshoe_2_20.R")

# Investigate convergence in these.
diag_cols <- c("min_rhat", "max_rhat", "divergent_trans")

summary(subset(horseshoe_2_3_results, select = diag_cols))
summary(subset(horseshoe_2_9_results, select = diag_cols))
summary(subset(horseshoe_2_20_results, select = diag_cols))

# Afterwards discard these columns, such that we can row bind with the other results.
horseshoe_2_3_results <- horseshoe_2_3_results |>
  select(-any_of(diag_cols))
horseshoe_2_9_results <- horseshoe_2_9_results |>
  select(-any_of(diag_cols))
horseshoe_2_20_results <- horseshoe_2_20_results |>
  select(-any_of(diag_cols)) |>
  filter(subgroup %in% all_subgroups)

# Combine analysis results and scenario properties ----

results <- rbind(
  population_results,
  subgroup_results,
  subtee_results,
  ridge_results,
  lasso_results,
  horseshoe_results,
  horseshoe_2_3_results,
  horseshoe_2_9_results,
  horseshoe_2_20_results
) |>
  full_join(scenario_properties, by = c("scenario_no", "subgroup"))

head(results)
summary(results)
