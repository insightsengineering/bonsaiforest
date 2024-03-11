test_that("generate_stacked_data outputs the right element for survival", {
  result <- generate_stacked_data(
    Surv(tt_pfs, ev_pfs) ~ arm, ~ x_1 + x_2,
    example_data, "survival"
  )
  data <- tibble::as_tibble(example_data[, c(
    "arm", "tt_pfs", "ev_pfs",
    "x_1", "x_2"
  )])
  data$x_1 <- as.character(data$x_1)
  data$x_2 <- as.character(data$x_2)
  d <- tidyr::gather(
    data, "subgroup_var", "subgroup_value",
    -"arm", -"tt_pfs", -"ev_pfs"
  )
  subgroup <- factor(paste(d$subgroup_var, d$subgroup_value, sep = "."),
    levels = c("x_1.a", "x_1.b", "x_2.a", "x_2.b")
  )
  d <- cbind(d, subgroup)
  d <- dplyr::arrange(d, subgroup)
  colnames(d) <- c(
    "arm", "time", "status", "subgroup_var", "subgroup_value",
    "subgroup"
  )
  expected <- d
  expect_equal(result, expected)
})

test_that("generate_stacked_data outputs the right element for binary", {
  result <- generate_stacked_data(
    ev_pfs ~ arm, ~ x_1 + x_2,
    example_data, "binary"
  )
  data <- tibble::as_tibble(example_data[, c(
    "arm", "ev_pfs",
    "x_1", "x_2"
  )])
  data$x_1 <- as.character(data$x_1)
  data$x_2 <- as.character(data$x_2)
  d <- tidyr::gather(
    data, "subgroup_var", "subgroup_value",
    -"arm", -"ev_pfs"
  )
  subgroup <- factor(paste(d$subgroup_var, d$subgroup_value, sep = "."),
    levels = c("x_1.a", "x_1.b", "x_2.a", "x_2.b")
  )
  d <- cbind(d, subgroup)
  d <- dplyr::arrange(d, subgroup)
  colnames(d) <- c(
    "arm", "y", "subgroup_var", "subgroup_value",
    "subgroup"
  )
  expected <- d
  expect_equal(result, expected)
})
