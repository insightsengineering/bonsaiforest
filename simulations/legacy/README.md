Directories:

- `scenarios`: `RData` files of the scenarios used in the six different methods files.

The six different methods:

- `naivepop_simulation_surv.R`: file with the functions to obtain the overall HR for the different scenarios.
- `naive_simulation_surv.R`: file with the functions to obtain the naive estimated HR for the different scenarios.
- `ridge_simulation_surv.R`: file with the functions to obtain the ridge estimated HR for the different scenarios.
- `lasso_simulation_surv.R`: file with the functions to obtain the lasso estimated HR for the different scenarios.
- `horseshoe_simulation_surv.R`: file with the functions to obtain the horseshoe estimated HR for the different scenarios. 
- `subtee_simulation_surv.R`: file with the functions to obtain the subtee estimated HR for the different scenarios.

Scenarios construction:

- `simulate_scenarios_surv.R`: file with the functions to simulate the six different scenarios used when having survival data.
- `true_parameters_surv.R`: file with the functions to obtain the true parameters of the models of each one of the scenarios.
- `true_parameters_surv.RData`: resulting saved true parameters.
- `Rfunctions_datasim_surv.R`: file with the functions to simulate survival data.
- `Rfunctions_naivefit_surv.R`: file with the functions to generative naive subgroup-specific treatment effect estimates for survival data.

Required results format:

- `sim_results_surv.Rdata`: long format data frame in the required results format.
