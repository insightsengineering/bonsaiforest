est_coef_bin1 <- as.matrix(coef(elastic_net_fit_bin$fit,
  s = elastic_net_fit_bin$fit$lambda.min
))
usethis::use_data(est_coef_bin1, overwrite = TRUE)
