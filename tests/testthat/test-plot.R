test_that("plot works as expected for naive", {
  result <- plot(summary(naive_fit_surv))
  vdiffr::expect_doppelganger("forest_plot_naive", result)
})

test_that("plot works as expected for elastic_net", {
  result <- plot(summary(elastic_net_fit_surv))
  vdiffr::expect_doppelganger("forest_plot_elastic_net", result)
})

test_that("plot works as expected for horseshoe", {
  result <- plot(summary(horseshoe_fit_bin))
  vdiffr::expect_doppelganger("forest_plot_horseshoe", result)
})

test_that("plot works as expected for compare.data", {
  result <- plot(compare(naive_fit_surv, naivepop_fit_surv, elastic_net_fit_surv))
  vdiffr::expect_doppelganger("forest_plot_comparison", result)
})
