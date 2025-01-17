naive_fit_surv <- naive(
  "tt_pfs", "arm", c(
    "x_1", "x_2", "x_3", "x_4", "x_5",
    "x_6", "x_7", "x_8", "x_9", "x_10"
  ),
  example_data, "survival", "ev_pfs"
)
usethis::use_data(naive_fit_surv, compress = "xz", overwrite = TRUE)
