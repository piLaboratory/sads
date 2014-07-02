\name{sads-package}
\alias{sads-package}
\alias{sads}
\docType{package}

\title{
  Modeling Species Abundance Distributions 
}

\description{
  Tools to fit probability distributions to  species abundance
  distributions (SADs) and species rank-abundance distributions (RADs)
  using maximum likelihood.
}
\details{
  \tabular{ll}{
    Package: \tab sads\cr
    Type: \tab Package\cr
    Version: \tab 0.1.10\cr
    Date: \tab 2014-07-01\cr
    License: \tab gpl \cr
    LazyLoad: \tab yes\cr
  }
  The distribution of abundances of species is one of the basic patterns of
  ecological communities. The empirical distributions of abundances (SADs)
  or their ranks (RADs) are traditionally
  modelled through probability distributions. Hence, the maximum
  likelihood method can be used to fit and compare competing models for
  SADs and RADs. The \pkg{sads} package provides functions, classes and methods to: 
  \itemize{
    \item Fit classic SAD models: log-series, lognormal, broken-stick, \dots ; 
    \item Fit classic rank-abundance (RADs) models: geometric,
    broken-stick, zipf, zipf-mandelbrodt, \dots ;
    \item Tools for quick diagnostic and comparison of models;
    \item Tools to simulate Poisson and Negative Binomial samples from
    abundances in communities.
  }
}

\author{
  Paulo I. Prado and Murilo Dantas Miranda

  Maintainer: P.I. Prado  <prado@ib.usp.br>
}

\references{
  Magurran, A.E. 2004. \emph{Measuring Biological Diversity}. Blackwell.

  Magurran, A.E. and McGill, B.J. 2011. \emph{Biological Diversity --
  Frontiers in measurement and assessment}. Oxford University Press.

  May, R.M. 1975. Patterns of Species Abundance and Diversity. In
  M. L. Cody and J. M. Diamond (Eds.), (pp. 81--120). Harvard University
  Press. 
  
  Green,J. and Plotkin, J.B. 2007 A statistical theory for sampling
  species abundances. \emph{Ecology Letters 10}:1037--1045.
}

\keyword{ species abundance distributions, ecology, environmetrics,
  maximum likelihood estimation}

\seealso{
  vignettes of \pkg{sads}; \code{vegan-package}, \code{poilog-package},
  \code{untb-package}.
}

