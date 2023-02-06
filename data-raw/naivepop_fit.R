naivepop_fit <- naivepop("tt_pfs", "arm", example_data, "survival", "ev_pfs")

usethis::use_data(naivepop_fit, overwrite = TRUE)
