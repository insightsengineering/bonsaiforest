test_that("print outputs the right element for naivepop", {
  result <- summary(naivepop_fit_bin)
  expect_snapshot(print(result))
})

test_that("print outputs the right element for naive", {
  result <- summary(naive_fit_bin)
  expect_snapshot(print(result))
})

test_that("print outputs the right element for elastic_net", {
  result <- summary(elastic_net_fit_bin)
  expect_snapshot(print(result))
})

test_that("summary outputs the right element for horseshoe", {
  result <- summary(horseshoe_fit_bin)
  expect_snapshot(print(result))
})
