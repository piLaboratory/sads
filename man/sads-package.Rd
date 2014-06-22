\name{sads-package}
\alias{sads-package}
\alias{sads}
\docType{package}
\title{
Modeling Species Abundance Distributions 
}
\description{
Tools to fit probability distributions to  species abundance
distributions and species rank-abundance distributions using maximum likelihood.
}
\details{
\tabular{ll}{
Package: \tab sads\cr
Type: \tab Package\cr
Version: \tab 0.1.09\cr
Date: \tab 2014-06-21\cr
License: \tab gpl \cr
LazyLoad: \tab yes\cr
}
Species abundance distributions (SADs) are one of the basic patterns of
ecological communities. The empirical distributions of abundances (SADs)
or their ranks (RADs) are traditionally
modelled through probability distributions. Hence, the maximum
likelihood method can be used to fit and compare competing models for
SADs. The \code{sads} package provides functions, classe and methods to: 
\itemize{
  \item Fit classic SAD models: logseries, lognormal, brokenstick, \dots  
  \item Fit classic rank-abundance (RADs) models: geometric, brokenstick, zipf,
  \item zipf-mandelbrodt, \dots
  \item Tools for the diagnostic and comparison of models 
  \item Tools to simulate Poisson and Negative Binomial samples from
  abundances in communities.
}
}

\author{
Paulo I Prado and Murilo Dantas Miranda

Maintainer: P.I. Prado  <prado@ib.usp.br>
}

\references{
  Magurran, A. E. 2004. Measuring Biological Diversity. Blackwell.

  Magurran, A. E. and McGill, B. J. 2011. Biological Diversity --
  Frontiers in measurement and assessment Oxford University Press.

  May, R. M. 1975. Patterns of Species Abundance and Diversity. In
  M. L. Cody and J. M. Diamond (Eds.), (pp. 81-120). Harvard University
  Press. 
  
  Green,J. and Plotkin, J.B. 2007 A statistical theory for sampling
  species abundances. \emph{Ecology Letters 10}:1037--1045.
}

\keyword{ species abundance distributions, ecology, environmetrics,
  maximum likelihood estimation}

\seealso{
vegan-package, poilog-package, untb-package
}

