elastic_net_fit_surv <- elastic_net("tt_pfs", "arm", c("x_1", "x_2", "x_3",
                                                       "x_4", "x_5", "x_6", "x_7",
                                                       "x_8", "x_9", "x_10"),
                                    c("x_1", "x_2", "x_3", "x_4", "x_5", "x_6",
                                      "x_7", "x_8", "x_9", "x_10"),
                                    example_data, "survival", 1, "ev_pfs")
usethis::use_data(elastic_net_fit_surv, compress = "xz", overwrite = TRUE)
