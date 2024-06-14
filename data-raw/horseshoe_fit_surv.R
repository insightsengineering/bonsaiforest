horseshoe_fit_surv <- horseshoe(
  resp = "tt_pfs",
  trt = "arm",
  subgr = c("x_1", "x_2"),
  covars = c(
    "x_1", "x_2", "x_3", "x_4", "x_5", "x_6",
    "x_7", "x_8", "x_9", "x_10"
  ),
  data = example_data,
  resptype = "survival",
  status = "ev_pfs",
  seed = 0,
  chains = 2,
  iter = 1000,
  warmup = 800,
  control = list(adapt_delta = 0.95)
)
usethis::use_data(horseshoe_fit_surv, compress = "xz", overwrite = TRUE)
