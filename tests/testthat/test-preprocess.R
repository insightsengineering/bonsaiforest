test_that("preprocess outputs the right elements", {
  result <- preprocess(
    "arm", c("x_1", "x_3"), c("x_1", "x_2", "x_3"),
    example_data
  )
  design_main <- stats::model.matrix(~ -1 + arm + x_1 + x_2 + x_3,
    data = example_data
  )
  design_dummy <- cbind(
    stats::model.matrix(~ -1 + x_1, data = example_data),
    stats::model.matrix(~ -1 + x_2, data = example_data),
    stats::model.matrix(~ -1 + x_3, data = example_data)
  )
  design_ia <- design_dummy * as.numeric(example_data$arm == 1)
  colnames(design_ia) <- paste(colnames(design_ia), "arm", sep = "_")
  subgr_names <- c("x_1a", "x_1b", "x_3a", "x_3b")
  expected <- list(
    design_ia = design_ia,
    design_main = design_main,
    design_dummy = design_dummy,
    subgr_names = subgr_names
  )
  expect_equal(result, expected)
})
