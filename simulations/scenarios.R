# Simulate data from the seven different scenarios used.

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

# For additional scenario 2_20:
# We extract the saved simulated data for original scenario 2
# and add 10 noise covariates each.
scen2 <- readRDS(file.path(out_dir, "scenario2.rds"))

set.seed(2041)
scen2_20 <- lapply(
  scen2,
  function(df) {
    add20 <- simul_covariates(
      nrow(df),
      p_catvar = 20,
      add_contvars = FALSE
    )
    add10 <- select(add20, "x_11":"x_20")
    cbind(df, add10)
  }
)

scen2_20_file <- file.path(out_dir, "scenario2_20.rds")
saveRDS(scen2_20, file = scen2_20_file)
