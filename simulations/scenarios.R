# Simulate data from the six different scenarios used.

library(tidyverse)
library(survival)
library(forestplot)
library(bonsaiforest)

out_dir <- "scenarios"
set.seed(0)

for (i in as.character(1:6)) {
  scen <- simul_scenario(i)
  scen_name <- paste0("scenario", i)
  scen_file <- file.path(out_dir, paste0(scen_name, ".rds"))
  saveRDS(scen, file = scen_file)
}
