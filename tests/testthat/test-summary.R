test_that("summary outputs the right element for naivepop survival", {
  result <- summary(naivepop_fit_surv)
  expected <- exp(coef(naivepop_fit_surv$fit))
  expect_equal(result, expected)
})

test_that("summary outputs the right element for naivepop binary", {
  result <- summary(naivepop_fit_bin)
  expected <- coef(naivepop_fit_bin$fit)[2]
  expect_equal(result, expected)
})

test_that("summary outputs the right element for naive survival", {
  result <- summary(naive_fit_surv)
  expected <- data.frame(
    subgroup = c(
      "x_1.a", "x_1.b", "x_2.a", "x_2.b", "x_3.a", "x_3.b", "x_4.a",
      "x_4.b", "x_4.c", "x_5.a", "x_5.b", "x_5.c", "x_5.d", "x_6.a",
      "x_6.b", "x_7.a", "x_7.b", "x_8.a", "x_8.b", "x_8.c", "x_9.a",
      "x_9.b", "x_10.a", "x_10.b", "x_10.c"
    ),
    trt.effect = c(
      0.6742, 0.6646, 0.7088, 0.6424, 0.5055, 0.7168, 0.6214,
      0.5429, 0.8576, 1.0715, 0.3981, 0.5966, 0.6819, 0.7501,
      0.6204, 0.4884, 0.7884, 0.5724, 0.5663, 0.7809, 0.4616,
      0.7165, 0.6845, 0.7063, 0.6366
    ),
    trt.conf.low = c(
      0.4694, 0.4664, 0.4744, 0.4637, 0.3004, 0.5366, 0.4133,
      0.3372, 0.5484, 0.5843, 0.1911, 0.3719, 0.4569, 0.4833,
      0.4552, 0.3137, 0.5775, 0.3253, 0.3506, 0.5485, 0.2455,
      0.5427, 0.3818, 0.4515, 0.4435
    ),
    trt.conf.high = c(
      0.9684, 0.9471, 1.059, 0.8901, 0.8507, 0.9577, 0.9341,
      0.8739, 1.3412, 1.9648, 0.8294, 0.9571, 1.0175, 1.164,
      0.8455, 0.7605, 1.0762, 1.0071, 0.9147, 1.1118, 0.8681,
      0.946, 1.2271, 1.1049, 0.9138
    )
  )
  expect_equal(result, expected, tolerance = 0.001)
})

test_that("summary outputs the right element for naive binary", {
  result <- summary(naive_fit_bin)
  expected <- data.frame(
    subgroup = c(
      "x_1.a", "x_1.b", "x_2.a", "x_2.b", "x_3.a", "x_3.b", "x_4.a",
      "x_4.b", "x_4.c", "x_5.a", "x_5.b", "x_5.c", "x_5.d", "x_6.a",
      "x_6.b", "x_7.a", "x_7.b", "x_8.a", "x_8.b", "x_8.c", "x_9.a",
      "x_9.b", "x_10.a", "x_10.b", "x_10.c"
    ),
    trt.effect = c(
      -0.4621, -0.4217, -0.3952, -0.4786, -0.7587, -0.3707,
      -0.5144, -0.6337, -0.2031, 0.0098, -1.0469, -0.5333, -0.387,
      -0.3174, -0.5261, -0.7501, -0.285, -0.6142, -0.5684, -0.309,
      -0.8926, -0.3509, -0.3762, -0.4055, -0.5053
    ),
    trt.conf.low = c(
      -0.8808, -0.8259, -0.8551, -0.8537, -1.3794, -0.7006,
      -1.0023, -1.1784, -0.7012, -0.6961, -1.8869, -1.0757,
      -0.8424, -0.8131, -0.8857, -1.2476, -0.6462, -1.2577,
      -1.1143, -0.7175, -1.5942, -0.6724, -1.0306, -0.9238,
      -0.9226
    ),
    trt.conf.high = c(
      -0.0433, -0.0176, 0.0647, -0.1034, -0.138, -0.0407,
      -0.0265, -0.0891, 0.2949, 0.7157, -0.2069, 0.0092,
      0.0683, 0.1782, -0.1665, -0.2526, 0.0763, 0.0293,
      -0.0225, 0.0994, -0.191, -0.0295, 0.2782, 0.1129, -0.088
    )
  )
  expect_equal(result, expected, tolerance = 0.001)
})


test_that("summary outputs the right element for elastic_net survival", {
  result <- summary(elastic_net_fit_surv)
  expected <- data.frame(
    subgroup = elastic_net_fit_surv$subgr_names,
    trt.estimate = c(
      0.65, 0.6494, 0.6494, 0.6502, 0.6498, 0.649, 0.6474,
      0.6467, 0.6467, 0.6498, 0.6496, 0.6494, 0.649, 0.6486,
      0.6494, 0.6489, 0.6498, 0.6497, 0.6496, 0.6499, 0.6487,
      0.6498, 0.6498, 0.6496, 0.6493
    )
  )
  expect_equal(result, expected, tolerance = 0.001)
})

test_that("summary outputs the right element for elastic_net binary", {
  result <- summary(elastic_net_fit_bin)
  expected <- data.frame(
    subgroup = elastic_net_fit_surv$subgr_names,
    trt.estimate = c(
      -0.463, -0.4641, -0.4641, -0.4627, -0.4641, -0.4656,
      -0.468, -0.4692, -0.4694, -0.4634, -0.4636, -0.4637,
      -0.4642, -0.4655, -0.4641, -0.4649, -0.4634, -0.4634,
      -0.4638, -0.4631, -0.4658, -0.4636, -0.4636, -0.464,
      -0.4645
    )
  )
  expect_equal(result, expected, tolerance = 0.001)
})


test_that("summary outputs the right element for horseshoe survival", {
  result <- summary(horseshoe_fit_surv)
  posterior <- trt_horseshoe(horseshoe_fit_surv)
  summary_post <- data.frame(
    subgroup = horseshoe_fit_surv$subgr_names,
    trt.median = c(
      0.6599, 0.653, 0.6577, 0.656, 0.6304,
      0.6662, 0.6444, 0.6445, 0.6701, 0.6887,
      0.6275, 0.6515, 0.6542, 0.6564, 0.6561,
      0.6204, 0.6785, 0.6359, 0.636, 0.6796,
      0.6375, 0.6633, 0.6506, 0.6585, 0.6579
    ),
    trt.cred.low = c(
      0.4968, 0.4911, 0.4876, 0.4969, 0.4091,
      0.5053, 0.4769, 0.4644, 0.4984, 0.5021,
      0.3401, 0.4743, 0.4818, 0.4809, 0.4956,
      0.4085, 0.5143, 0.4418, 0.4484, 0.5127,
      0.3869, 0.5082, 0.4668, 0.4924, 0.4949
    ),
    trt.cred.high = c(
      0.8661, 0.8561, 0.8778, 0.8574, 0.8471,
      0.8726, 0.8513, 0.8596, 0.9478, 1.2081,
      0.8558, 0.8699, 0.8677, 0.9133, 0.8514,
      0.8395, 0.9036, 0.8616, 0.8472, 0.9172,
      0.8634, 0.863, 0.8791, 0.8853, 0.8607
    )
  )
  expected <- list(
    posterior = posterior,
    summary_post = summary_post
  )
  expect_equal(result, expected, tolerance = 0.001)
})


test_that("summary outputs the right element for horseshoe binary", {
  result <- summary(horseshoe_fit_bin)
  posterior <- trt_horseshoe(horseshoe_fit_bin)
  summary_post <- data.frame(
    subgroup = horseshoe_fit_surv$subgr_names,
    trt.median = c(
      -0.4633, -0.467, -0.4665, -0.4646, -0.5009,
      -0.4543, -0.4785, -0.4804, -0.4518, -0.4323,
      -0.5206, -0.4689, -0.4556, -0.475, -0.4644,
      -0.5204, -0.4338, -0.4949, -0.4847, -0.4391,
      -0.5097, -0.4503, -0.4749, -0.4636, -0.468
    ),
    trt.cred.low = c(
      -0.7766, -0.7841, -0.7985, -0.7675, -0.9803,
      -0.7566, -0.8233, -0.8547, -0.7796, -0.7773,
      -1.2444, -0.8131, -0.7818, -0.8101, -0.7719,
      -0.9656, -0.746, -0.9154, -0.8507, -0.7549,
      -1.1072, -0.7497, -0.8368, -0.8013, -0.7792
    ),
    trt.cred.high = c(
      -0.1563, -0.1635, -0.1476, -0.162, -0.1583,
      -0.1513, -0.1592, -0.1524, -0.0998, 0.0454,
      -0.1698, -0.1386, -0.1305, -0.1126, -0.1641,
      -0.1858, -0.0937, -0.1575, -0.1677, -0.112,
      -0.1563, -0.1444, -0.1148, -0.116, -0.1576
    )
  )
  expected <- list(
    posterior = posterior,
    summary_post = summary_post
  )
  expect_equal(result, expected, tolerance = 0.001)
})
