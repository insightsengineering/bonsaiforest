# Main script to run the simulations.
library(checkmate)
library(bonsaiforest)
library(parallel)
library(parallelly)

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

# Run analyses ----

source("naivepop.R")
source("ridge.R")

