test_that("test works as expected", {
  result <- test(bla = "a", bli = c(1, NA), blu = list(1, 2, 3))
  expected <- list("a", c(1, NA), list(1, 2, 3))
  expect_identical(result, expected)
})
