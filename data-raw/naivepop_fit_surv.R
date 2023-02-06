naivepop_fit_surv <- naivepop("tt_pfs", "arm", example_data, "survival", "ev_pfs")

usethis::use_data(naivepop_fit_surv, overwrite = TRUE)
