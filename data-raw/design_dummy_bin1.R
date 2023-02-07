ind <- which(elastic_net_fit_bin$design_dummy[, 1] == 1)
design_dummy_bin1 <- elastic_net_fit_bin$design_dummy[ind, ]
usethis::use_data(design_dummy_bin1, overwrite = TRUE)
