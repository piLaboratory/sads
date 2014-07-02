\name{dpareto}
\alias{dpareto}
\alias{ppareto}
\alias{qpareto}

\title{Pareto distribution}

\description{
  Density, distribution function and quantile function for
  the Pareto distribution with parameters \code{shape} and \code{scale}.
}

\usage{
dpareto(x, shape, scale = min(x), log = FALSE)
ppareto(q, shape, scale = min(q), lower.tail = TRUE, log.p = FALSE)
qpareto(p, shape, scale = min(q), lower.tail = TRUE, log.p = FALSE)
}

\arguments{
\item{x}{
    vector of (non-negative integer) quantiles. In the context of
    species abundance distributions, this is a vector of abundances of species in a sample.
  }
  \item{q}{vector of (non-negative integer) quantiles. In the context of
    species abundance distributions, a vector of
    abundances of species in a sample.
  }
  \item{p}{
    vector of probabilities.
  }
  \item{shape}{
    positive real; shape parameter, a.k.a Pareto's index or tail index.
  }
  \item{scale}{
    positive real, scale >= min(x); scale parameter.
  }
  \item{log, log.p}{
    logical; if TRUE, probabilities p and densities d are given as
log(p) and log(d).
  }
  \item{lower.tail}{
    logical; if TRUE (default), probabilities are P[X <= x],
    otherwise, P[X > x].
  }
}

\value{
\code{dpareto} gives the (log) density, \code{ppareto} gives the (log)
distribution function, \code{qpareto} gives the quantile function.

Invalid values for parameters \code{shape} or \code{scale} will result in return
values \code{NaN}, with a warning.
}

\details{
The Pareto distribution is a continuous power-law density distribution
with \code{scale} and \code{shape} parameters that has the form:

f(x) = shape * scale^shape / x^(shape+1)

For all x >= scale, and

f(x) = 0

otherwise.

The shape parameter is known as Pareto's index or tail index, and
increases the decay of f(x). This distribution was originally used to
describe the allocation of wealth or income among individuals in human
societies. As a continuous counterpart of Zipf
Law, Pareto distribution describes well many other variables that follow
a power-law.

In ecology the Pareto distribution can be used to describe the distribution
of abundances among species in a biological assemblage
(a.k.a. biological community) or in a sample taken from such an
assemblage. Though much less used than the lognormal to fit SADs, it can fit better the extremities
of the empirical distributions to which the lognormal applies (Johnson et al. 1995, p.608).
}

\note{

  These functions implement the Pareto distribution of the first kind sensu
  Johnson et al. (1995, pp.574).
  
  The pdf and cdf are defined as zero for all \code{x < scale}, but the
  functions \code{[dp]pareto} currently return an error if \code{scale > min(x)}, to avoid some
  fitting and plotting problems.
}

\references{
  Johnson, N.L., Kotz, S. and Balakrishnan, N. 1995. \emph{Continuous
    Univariate Distributions}, volume 2, chapter 20.  Wiley, New York.
}

\seealso{
  \code{Pareto} in packages \pkg{VGAM} and \pkg{actuar} for more general
  and flexible implementations; \code{\link{fitpareto}} for maximum
  likelihood estimation in the context of species abundance
  distributions.
}

\author{Paulo I Prado \email{prado@ib.usp.br} and Murilo Dantas Miranda.}

\examples{
par(mfrow=c(1,2))
curve(dpareto(x, shape=3, scale=1), 1,8, ylab="Density",
      main="Pareto PDF")
curve(ppareto(x, shape=3, scale=1), 1,8, ylab="Probability",
      main="Pareto CDF")
par(mfrow=c(1,1))


## Quantile is the inverse function of probability:
p.123 <-ppareto(1:3,shape=3,scale=0.99) 
all.equal(qpareto(p.123, shape=3, scale=0.99), 1:3) 
}



