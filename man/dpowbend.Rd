\name{dpowbend}
\alias{dpowbend}
\alias{ppowbend}
\alias{qpowbend}

\title{Puyeo's Power-bend discrete distribution
}

\description{
Density, distribution function and quantile function for discrete
version of Pueyo's power-bend distribution with parameters \code{s} and \code{omega}.
}

\usage{
dpowbend( x, s, omega, log=FALSE)
ppowbend ( q, s, omega, lower.tail=TRUE, log.p=FALSE)
qpowbend( p, s, omega, lower.tail= TRUE, log.p=FALSE)
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
  \item{p}{
    vector of probabilities.
  }
  \item{s}{
    positive real s > 1; exponent of the power-bend distribution.
  }
  \item{omega}{
	positive real greater than 0; bending parameter of the distribution. 
	The current implementation only accepts omega smaller than 5.
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
  The power-bend density is a discrete probability distribution based on the
 power probability density with an added term for ``bending'', defined for
  integer x > 0:

  \deqn{p(x) = \frac{x^{-s} e^{- \omega x}}{Li_s (e^\omega)}}{p(x) = x^(-s)exp(-omega x) / Li_s (exp(omega))}

  This distribution was proposed by S. Pueyo to represent a middle-ground between the 
  logseries and the exponential distribution. 
  
  The function \eqn{Li_s(e^\omega)}{Li_s(exp(omega))} is the Polylogarithm of the 
  exponential of \code{omega} on base \code{s}, and represents the integration constant.
  A naive implementation of the Polylogarithm function is included in the package, and
  accepts values for non-integer \code{s} and \code{omega} smaller than 5, which cover the
  cases of biological relevance. 
 }

\value{
  \code{dpowbend} gives the (log) density of the function, \code{ppowbend} gives the (log)
  distribution function, \code{qpowbend} gives the quantile function.

  Invalid values for parameter \code{s} or \code{omega} will result in return
  values \code{NaN}, with a warning. Note that integer values of \code{s} and 
  \code{omega} values larger than 5 are currently not supported, and will also
  return \code{NaN}.
}

\references{
  Pueyo, S. (2006) \emph{Diversity: Between neutrality and structure},
  OIKOS 112: 392-405. 
}

\author{Paulo I Prado \email{prado@ib.usp.br} and Andre Chalom.}

\seealso{
\code{dpower} for the power distribution; \code{\link{fitpowbend}} for maximum likelihood estimation in
the context of species abundance distributions. 
}

\examples{
x <- 1:20
PDF <- dpowbend(x=x, s=2.1, omega=0.5)
CDF <- ppowbend(q=x, s=2.1, omega=0.5)
par(mfrow=c(1,2))
plot(x,CDF, ylab="Cumulative Probability", type="b",
     main="Power distribution, CDF")
plot(x,PDF, ylab="Probability", type="h",
     main="Power distribution, PDF")
par(mfrow=c(1,1))

## The powbend distribution is a discrete PDF, hence:
all.equal( ppowbend(10, s=2.1, omega=0.5), sum(dpowbend(1:10, s=2.1, omega=0.5)) ) # should be TRUE

## quantile is the inverse of CDF
all.equal(qpowbend(CDF, s=2.1, omega=0.5), x) 
}
