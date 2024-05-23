# Start here:

- `run.R`: Main script to run the simulations.

# Other files:

- `horseshoe.R`, `lasso.R`, `naive.R`, `naivepop.R`, `ridge.R`, `subtee.R`: Scripts producing the corresponding methods' results.
- `functions.R`: Few central helper functions.
- `scenarios.R`: Simulate data from the six different scenarios used.
- `truth.R`: True (average) hazard ratios in the six different scenarios.

# Directories:

- `legacy`: Old simulation scripts.
- `results`: Cached results files from simulation runs.
- `scenarios`: Saved data sets for the six different scenarios, produced by `scenarios.R`,
  as well as true parameters produced by `truth.R`.
