\name{dpoig}
\alias{dpoig}

\title{Compound Poisson-gamma distribution}

\description{
  Density for the Poisson-gamma compound probability distribution with
  parameters \code{frac}, \code{rate} and \code{rate}.
}

\usage{
dpoig(x, frac, rate, shape, log=FALSE)
}

\arguments{
  \item{x}{
    vector of (non-negative integer) quantiles.
  }
  \item{frac}{
    single numeric '0<frac<=1'; fraction of the population or community sampled (see details)
  }
  \item{rate}{
    vector of (non-negative) rates of the gamma distribution of the
    sampled population (see details). Must be strictly positive.
  }
  \item{shape}{
    the shape parameter of the gamma distribution of the
    sampled population (see details).  Must be positive.
  }
  \item{log}{
    logical; if TRUE, probabilities p are given as log(p)
  }
}

\details{
  A compound Poisson-gamma distribution is a Poisson probability distribution
  where its single parameter, the process mean rate, is frac*n, where n
  is a random variable with gamma distribution. The density function
  is given by Green & Plotkin (2007) 

  In ecology, this distribution gives the probability that a species has
  an abundance of x individuals in a random sample of a fraction frac of
  the community. In the community the species abundances are independent
  random variables that follow an gamma density function.

  Hence, a Poisson-gamma distribution is a model for species
  abundances distributions (SAD) under the assumptions that (a) species
  abundances in the community are independent identically distributed
  gamma variables, (b) sampling is a Poisson process with expected
  value frac*n, (c) the sampling is done with replacement, or the fraction
  sampled is small enough to approximate a sample with replacement.

  The Poisson-gamma distribution is also known as the Negative Binomial
  distribution. The function dpoig is provided to express the Negative
  Binomial explicitly as a compound distribution.
  The Fisher log-series (Fisher 1943) is a limiting case
  where the dispersion parameter of the Negative Binomial tends to zero.

}
\value{
  (log) density of the (zero-truncated) density
}

\references{

  Fisher, R.A, Corbert, A.S. and Williams, C.B. (1943) The Relation
  between the number of species and the number of individuals in a
  random sample of an animal population. \emph{The Journal of Animal
    Ecology, 12}:42--58. 

  Green,J. and Plotkin, J.B. 2007 A statistical theory for sampling
  species abundances. \emph{Ecology Letters 10}:1037--1045
  
  Pielou, E.C. 1977. \emph{Mathematical Ecology}. New York: John Wiley
  and Sons.
  
}

\author{Paulo I Prado \email{prado@ib.usp.br} and Cristiano Strieder.}

\seealso{
  dgamma, dpois, dnbinom
}
