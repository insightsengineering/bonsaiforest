
<!-- markdownlint-disable-file -->
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bonsaiforest <a href="https://insightsengineering.github.io/bonsaiforest"><img src="man/figures/logo.png" align="right" height="88" alt="bonsaiforest website" /></a>

<!-- badges: start -->

[![Project Status: Initial development is in progress, but there has not
yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![CRAN
status](https://www.r-pkg.org/badges/version-last-release/bonsaiforest)](https://www.r-pkg.org/badges/version-last-release/bonsaiforest)
[![CRAN monthly
downloads](https://cranlogs.r-pkg.org/badges/bonsaiforest)](https://cranlogs.r-pkg.org/badges/bonsaiforest)
[![CRAN total
downloads](https://cranlogs.r-pkg.org/badges/grand-total/bonsaiforest)](https://cranlogs.r-pkg.org/badges/grand-total/bonsaiforest)
[![Code
Coverage](https://raw.githubusercontent.com/openpharma/bonsaiforest/_xml_coverage_reports/data/main/badge.svg)](https://raw.githubusercontent.com/openpharma/bonsaiforest/_xml_coverage_reports/data/main/coverage.xml)
<!-- badges: end -->

Subgroup analyses are routinely performed in clinical trial analyses.
From a methodological perspective, two key issues of subgroup analyses
are multiplicity (even if only predefined subgroups are investigated)
and the low sample sizes of subgroups which lead to highly variable
estimates. `bonsaiforest` implements subgroup estimates based on
Bayesian shrinkage priors, as well as penalized likelihood inference.

## Installation

### Development

You can install the development version of `bonsaiforest` from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("insightsengineering/bonsaiforest")
```

## Getting started

See the [introductory
vignette](https://insightsengineering.github.io/bonsaiforest/main/articles/introduction.html)
or get started by trying out the example:

``` r
library(bonsaiforest)
str(example_data)

horseshoe_model <- horseshoe(
  resp = "tt_pfs", trt = "arm",
  subgr = c("x_1", "x_2", "x_3", "x_4"),
  covars = c(
    "x_1", "x_2", "x_3", "x_4", "x_5",
    "x_6", "x_7", "x_8", "x_9", "x_10"
  ),
  data = example_data, resptype = "survival",
  status = "ev_pfs", chains = 2, seed = 0,
  control = list(adapt_delta = 0.95)
)

summary_horseshoe <- summary(horseshoe_model, conf = 0.9)
summary_horseshoe

plot(summary_horseshoe)
```
