naivepop_fit_bin <- naivepop("ev_pfs", "arm", example_data, "binary")

usethis::use_data(naivepop_fit_bin, compress = "xz", overwrite = TRUE)
