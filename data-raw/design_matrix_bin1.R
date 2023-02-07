ind <- which(elastic_net_fit_bin$design_dummy[, 1] == 1)
design_matrix_bin1 <- elastic_net_fit_bin$design_matrix[ind, ]
usethis::use_data(design_matrix_bin1, overwrite = TRUE)
