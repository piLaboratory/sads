\name{dpoix}

\alias{dpoix}
\alias{ppoix}
\alias{qpoix}
\alias{rpoix}

\title{Compound Poisson-Exponential distribution
}

\description{
  Density, distribution function, quantile function and random generation
  for the Poisson-exponential compound probability distribution
  with parameters \code{fraction} and \code{rate}.
}

\usage{
dpoix(x, frac, rate, log=FALSE)
ppoix(q, frac, rate, lower.tail=TRUE, log.p=FALSE)
qpoix(p, frac, rate, lower.tail=TRUE, log.p=FALSE)
rpoix(n, frac, rate)
}

\arguments{
  \item{x}{
    vector of (non-negative integer) quantiles. In the context of
    species abundance distributions, this is a vector of abundances 
    of species in a sample.
  }
  \item{q}{vector of (non-negative integer) quantiles. In the context of
    species abundance distributions, a vector of abundances of species in a sample.
  }
  \item{n}{number of random values to return.}
  \item{p}{
    vector of probabilities.
  }
  \item{frac}{
    single numeric 0 < frac <= 1; fraction of the population or community sampled (see details).
  }
  \item{rate}{
    vector of (non-negative) rates of the exponential distribution of the
    sampled population (see details).
  }
  \item{log, log.p}{
    logical; if TRUE, probabilities p are given as log(p)
  }
  \item{lower.tail}{
    logical; if TRUE (default), probabilities are P[X <= x],
    otherwise, P[X > x].
  }
}
\details{
  A compound Poisson-exponential distribution is a Poisson probability distribution
  where its single parameter lambda, is frac*n, at which n
  is a random variable with exponential distribution. Thus, the expected
  value and variance are E[X] = Var[X] = frac*n . The density function is

  p(y) = rate*frac^y / (frac + rate)^(y+1)

  for x = 0, 1, 2, \ldots (Green & Plotkin 2007) In ecology, this
  distribution gives the probability that a species has
  an abundance of x individuals in a random sample of a fraction \code{frac}
  of the community. In the community the species abundances are
  independent random variables that follow an exponential density
  function.  

  Hence, a Poisson-exponential distribution is a model for species
  abundances distributions (SAD) in a sample taken from a community under the assumptions: (a) species
  abundances in the community are independent identically distributed
  exponential variables, (b) sampling is a Poisson process with expected
  value 'frac*n', (c) individuals are sampled with replacement, or the
  fraction of total individuals sampled is small enough to approximate a
  sample with replacement. See Engen (1977) and Alonso et al. (2008) for critic evaluations.

  Notice that the Poisson-exponential can be seen as a different form for the
  MacArthur's Broken stick model (Baczkowski, 2000), so instead of fitting to a 
  Poisson-exponential distribution directly, the user should use \code{fitbs}.

}
\value{
  (log) density of the (zero-truncated) density.
}

\references{
  Alonso, D. and Ostling, A., and Etienne, R.S. 2008. The implicit
  assumption of symmetry and the species abundance
  distribution. \emph{Ecology Letters, 11}: 93--105.

  Engen, S. 1977. Comments on two different approaches to the analysis
  of species frequency data. \emph{Biometrics, 33}: 205--213.

  Pielou, E.C. 1977. \emph{Mathematical Ecology}. New York: John Wiley
  and Sons.
  
  Green,J. and Plotkin, J.B. 2007 A statistical theory for sampling
  species abundances. \emph{Ecology Letters 10}:1037--1045 
}

\author{Paulo I Prado \email{prado@ib.usp.br}, Cristiano Strieder and Andre Chalom.}

\seealso{
  dexp, dpois for related distributions, dpoig for the general case of the Poisson-Gamma distribution
}

