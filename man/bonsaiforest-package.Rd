% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bonsaiforest-package.R
\docType{package}
\name{bonsaiforest-package}
\alias{bonsaiforest}
\alias{bonsaiforest-package}
\title{bonsaiforest: Shrinkage Based Forest Plots}
\description{
\if{html}{\figure{logo.png}{options: style='float: right' alt='logo' width='120'}}

Subgroup analyses are routinely performed in clinical trial analyses. This package implements shrinkage methods to estimate treatment effects in overlapping subgroups with a binary or time-to-event endpoint as described in Wolbers et al (2024) \href{https://arxiv.org/abs/2407.11729}{arXiv:2407.11729}. Both Bayesian estimation with a regularized horseshoe prior as well as penalized frequentist methods using the lasso or ridge penalties are implemented. The Bayesian approach provides both point estimates and credible intervals whereas only point estimates are available for the penalized frequentist methods. The estimators are intended to completement standard subgroup-specific estimators which are routinely displayed in forest plots. They typically have substantially smaller overall mean squared error compared to the standard estimator.
}
\seealso{
Useful links:
\itemize{
  \item \url{https://github.com/insightsengineering/bonsaiforest/}
  \item Report bugs at \url{https://github.com/insightsengineering/bonsaiforest/issues}
}

}
\author{
\strong{Maintainer}: Isaac Gravestock \email{isaac.gravestock@roche.com}

Authors:
\itemize{
  \item Mar Vazquez Rabunal \email{mar.vazquez_rabunal@roche.com}
  \item Daniel Sabanés Bové \email{daniel.sabanes_bove@roche.com}
  \item Marcel Wolbers \email{marcel.wolbers@roche.com}
}

Other contributors:
\itemize{
  \item F. Hoffmann-La Roche AG [copyright holder, funder]
}

}
\keyword{internal}
