# Simulate data from the six different scenarios used.

library(tidyverse)
library(survival)
library(forestplot)
library(bonsaiforest)

out_dir <- "scenarios"
set.seed(0)

for (i in as.character(1:6)) {
  tmp <- simul_scenario(i)
  scen_name <- paste0("scenario", i)
  assign(scen_name, tmp)
  save(list = scen_name, file = file.path(out_dir, paste0(scen_name, ".RData")))
}
