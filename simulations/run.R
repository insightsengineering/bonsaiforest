# Main script to run the simulations.
library(checkmate)
library(bonsaiforest)
library(parallel)
library(parallelly)
library(subtee) # Install from https://cran.r-project.org/src/contrib/Archive/subtee/subtee_1.0.1.tar.gz
library(cmdstanr) # Install as described at https://mc-stan.org/cmdstanr/articles/cmdstanr.html

# Load scenarios ----
scenario_files <- dir("legacy/scenarios")
assert_character(scenario_files, min.len = 1L)
scenarios <- new.env()
for (f in scenario_files) {
  load(file.path("legacy/scenarios", f), scenarios)
}
scenarios <- as.list(scenarios)
str(as.list(scenarios),1)
# List of 6
# $ data_scenario1:List of 8
# $ data_scenario2:List of 8
# $ data_scenario3:List of 8
# $ data_scenario4:List of 8
# $ data_scenario5:List of 8
# $ data_scenario6:List of 8

# Additional functions ----

source("functions.R")

# Run analyses ----

# df <- scenarios[[1]]$scenario[[1]]
source("population.R") # fast.
source("subgroup.R")    # fast.
source("subtee.R")   # takes a few minutes.
source("ridge.R")    # takes a few minutes.
source("lasso.R")    # takes a few minutes.
source("horseshoe.R") # this takes the longest time.
