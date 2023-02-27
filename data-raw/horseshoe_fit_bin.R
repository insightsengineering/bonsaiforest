horseshoe_fit_bin <- horseshoe(
  "ev_pfs", "arm", c("x_1", "x_2"),
  c(
    "x_1", "x_2", "x_3", "x_4", "x_5", "x_6",
    "x_7", "x_8", "x_9", "x_10"
  ),
  example_data, "binary",
  seed = 0, control = list(adapt_delta = 0.95)
)
usethis::use_data(horseshoe_fit_bin, compress = "xz", overwrite = TRUE)
