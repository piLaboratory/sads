\name{dls}
\alias{dls}
\alias{pls}
\alias{qls}
\title{Fisher's Log-series distribution}

\description{
Density, distribution function and quantile function for the Fisher's
log-series probability distribution with parameter \code{alpha}.
}
\usage{
dls( x, N , alpha, log=FALSE)
pls(q, N, alpha, lower.tail=TRUE, log.p=FALSE)
qls(p, N, alpha, lower.tail = TRUE, log.p = FALSE)
}

\arguments{
  \item{x}{
    vector of (integer, x>0) quantiles. Usually a vector of abundances of species in a sample.
  }
  \item{q}{vector of (integer, x>0) quantiles. Usually a vector of
    abundances of species in a sample.
  }
  \item{p}{
    vector of probabilities.
    }
  \item{N}{
    sample size. Usually the total number of individuals in the sample (see details).
  }
  \item{alpha}{
    real positive; Fisher's alpha parameter (see details).
  }
  \item{log, log.p}{
    logical; if TRUE, probabilities p are given as log(p).
  }
  \item{lower.tail}{
    logical; if TRUE (default), probabilities are P[X <= x],
    otherwise, P[X > x].
    }
}

\details{

  The Fisher log-series is a limiting case of the Negative Binomial where
  the dispersion parameter of the negative binomial tends to zero. It
  was originally proposed by Fisher (1943) to relate the expected number
  of species in a sample from a biological community to the sample size as:

  S = alpha * log(1 + N/alpha)

  Where alpha is the single parameter of the log-series distribution,
  often used as a diversity index. From this relation follows that the
  expected number of species with x individuals in the sample is
  
  S(x) = alpha*X^x/x

  Where X is a function of alpha and N, that tends to one as the sample
  size N increases:

  X = N / (alpha+ N)
  
  The density function used here is derived by Alonso et al. (2008,
  supplementary material). In ecology, this density distribution gives
  the probability that a species has
  an abundance of x individuals in a random sample of size N of the
  community. In the community the species abundances are independent
  random variables that follow a log-series distribution. Thus, a random
  sample of a log-series is also a log-series distribution.
  
  Hence, a log-series distribution is a model for species
  abundances distributions (SAD) under the assumptions that (a) species
  abundances in the community are independent identically distributed
  log-series variables, (b) sampling is a Poisson process, (c)
  sampling is done with replacement, or the fraction
  sampled is small enough to approximate a sample with replacement.
}

\value{
\code{dls} gives the (log) of the density, \code{pls} gives the (log)
distribution function, \code{qls} gives the (log) the quantile function.

Invalid values for parameter \code{alpha} will result in return
  values \code{NaN}, with a warning.
}
\references{

  Alonso, D. and Ostling, A., and Etienne, R. S. 2008 The implicit
  assumption of symmetry and the species abundance
  distribution. Ecology Letters, 11: 93-105.
  
  Fisher, R.A, Corbert, A.S. and Williams, C.B. (1943) The Relation
  between the number of species and the number of individuals in a
  random sample of an animal population. The Journal of Animal Ecology,
  12(1): 42--58.

  Green,J. and Plotkin, J.B. 2007 A statistical theory for sampling
  species abundances. Ecology Letters 10:1037--1045

  Pielou, E.C. 1977. Mathematical Ecology. New York: John Wiley
  and Sons.

}

\author{Paulo I Prado \email{prado@ib.usp.br} and Murilo Dantas Miranda.}

\seealso{
  \code{dpois}, \code{dnbinom}, \code{\link{dpoig}}.
  For maximum likelihood estimation in the context of species
  abundance distributions see \code{\link{fitls}}, \code{fisherfit} in \pkg{vegan} package
  and \code{fisher} in \pkg{untb} package.
}

\examples{
x <- 1:100
PDF <- dls(x=x, N=100, alpha=5)
CDF <- pls(q=x, N=100, alpha=5)
par(mfrow=c(1,2))
plot(x,CDF, ylab="Cumulative Probability", type="b",
     main="Log-Series distribution, CDF")
plot(x,PDF, ylab="Probability", type="h",
     main="Log-Series distribution, PDF")
par(mfrow=c(1,1))

## Fisher log-series is a discrete PDF, hence:
all.equal(pls(10,N=1000,alpha=50), sum(dls(1:10,N=1000,alpha=50))) # should be TRUE

## qls is the inverse of pls
all.equal(qls(CDF,N=100,alpha=5), x) # should be TRUE
  }
