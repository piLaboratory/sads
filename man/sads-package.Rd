\name{sads-package}
\alias{sads-package}
\alias{sads}
\docType{package}
\title{
\packageTitle{sads}
}
\description{
\packageDescription{sads}
}

\details{

The distribution of abundances of species is one of the basic patterns of
  ecological communities. The empirical distributions of abundances (SADs)
  or their ranks (RADs) are traditionally
  modelled through probability distributions. Hence, the maximum
  likelihood method can be used to fit and compare competing models for
  SADs and RADs. The \pkg{sads} package provides functions, classes and methods to: 
  \itemize{
    \item Fit classic SAD models: log-series, lognormal, broken-stick, \dots ; 
    \item Fit classic rank-abundance (RADs) models: geometric,
    broken-stick, Zipf, Zipf-Mandelbrodt, \dots ;
    \item Tools for quick diagnostic and comparison of models;
    \item Tools to simulate Poisson and Negative Binomial samples from
    abundances in communities.  
  }
}

\author{
\packageAuthor{sads}

Maintainer: \packageMaintainer{sads}
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

  Saether, B.E., Engen, S. and Grotan, V. 2013. Species diversity and
  community similarity in fluctuating environments: parametric
  approaches using species abundance distributions.
  \emph{Journal of Animal Ecology, 82}(4): 721--738.
}

\keyword{distribution}
\keyword{models}
\concept{species abundance distributions}
\concept{ecology}
\concept{environmetrics}
\concept{maximum likelihood estimation}



\seealso{
  vignettes of \pkg{sads}; \code{vegan-package} and \code{poilog-package}
}

\examples{
## Rank-abundance plot
plot( rad(moths) )
## Preston's plots
plot (octav(moths) )
## Fit logseries model
moths.ls <-  fitsad(moths, sad = 'ls')
## Diagnostic plots
par(mfrow=c(2,2))
plot(moths.ls)
par(mfrow = c(1,1))
## Model summary
summary(moths.ls)
## Confidence interval
confint(moths.ls)
}
