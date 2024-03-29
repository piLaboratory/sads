\name{dpower}
\alias{dpower}
\alias{ppower}
\alias{qpower}
\alias{rpower}

\title{Power discrete distribution
}

\description{
Density, distribution function, quantile function and random generation for discrete
version of power distribution with parameter \code{s}.
}

\usage{
dpower( x, s, log=FALSE)
ppower( q, s, lower.tail=TRUE, log.p=FALSE)
qpower( p, s, lower.tail= TRUE, log.p=FALSE)
rpower( n, s, bissection = FALSE, ...)
}

\arguments{
  \item{x}{
    vector of (integer x>0) quantiles. In the context of
    species abundance distributions, this is a vector of abundances of
    species in a sample. 
  }
  \item{q}{vector of (integer x>0) quantiles. In the context of
    species abundance distributions, a vector of
    abundances of species in a sample.
  }
  \item{n}{number of random values to return.}
  \item{p}{
    vector of probabilities.
  }
  \item{s}{
    positive real s > 1; exponent of the power distribution.
  }
  \item{log, log.p}{
    logical; if TRUE, probabilities p are given as log(p).
  }
  \item{lower.tail}{
    logical; if TRUE (default), probabilities are P[X <= x],
    otherwise, P[X > x].
  }
  \item{bissection}{
    logical; uses the bissection method to generate random numbers? If
    FALSE calls \code{poweRlaw::rpldis}, which provides a faster, continuous
    approximation for large numbers (Gilliespie 2015). 
  }

  \item{\dots}{further arguments to be passed to \code{poweRlaw::rpldis}
    when \code{bissection = TRUE}} 
  
}


\details{
  The power density is a discrete probability distribution defined for
  integer x > 0:

  \deqn{p(x) = \frac{x^{-s}}{\zeta (s)}}{p(x) = x^(-s) / zeta (s)}

  Hence p(x) is proportional to a
  negative power of 'x', given by the 's' exponent. The Riemann's \eqn{\zeta}{zeta}
  function is the integration constant.

  The power distribution can be used as a species abundance distribution (sad) model, which
  describes the probability of the abundance 'x' of a given species in a
  sample or assemblage of species.
 }

\value{
  \code{dpower} gives the (log) density of the density, \code{ppower} gives the (log)
  distribution function, \code{qpower} gives the quantile function.

  Invalid values for parameter \code{s} will result in return
  values \code{NaN}, with a warning.
}

\references{
  Johnson N. L., Kemp, A. W. and Kotz S. (2005) \emph{Univariate Discrete
  Distributions}, 3rd edition, Hoboken, New Jersey: Wiley. Section
  11.2.20.

  Colin S. Gillespie (2015) Fitting Heavy Tailed Distributions: The
  poweRlaw Package. \emph{Journal of Statistical Software, 64(2)}, 1-16. URL
  http://www.jstatsoft.org/v64/i02/.
}

\author{Paulo I Prado \email{prado@ib.usp.br} and Murilo Dantas Miranda.}

\seealso{
\code{dzeta} in \pkg{VGAM} package; \pkg{poweRlaw} package;
\code{\link{fitpower}} for maximum likelihood estimation in the context of species abundance distributions. 
}

\examples{
x <- 1:20
PDF <- dpower(x=x, s=2)
CDF <- ppower(q=x, s=2)
par(mfrow=c(1,2))
plot(x,CDF, ylab="Cumulative Probability", type="b",
     main="Power distribution, CDF")
plot(x,PDF, ylab="Probability", type="h",
     main="Power distribution, PDF")
par(mfrow=c(1,1))

## The power distribution is a discrete PDF, hence:
all.equal( ppower(10, s=2), sum(dpower(1:10, s=2)) ) # should be TRUE

## quantile is the inverse of CDF
all.equal(qpower(CDF, s=2), x) 

}
