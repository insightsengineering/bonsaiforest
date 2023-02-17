test_that("compareplot works as expected", {
  result <- compareplot(summary(naive_fit_bin), summary(naivepop_fit_bin),
                         summary(elastic_net_fit_bin))
  vdiffr::expect_doppelganger("forest_plot_comparison", result)
})
