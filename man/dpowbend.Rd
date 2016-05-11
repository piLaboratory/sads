\name{dpowbend}
\alias{dpowbend}
\alias{ppowbend}
\alias{qpowbend}
\alias{rpowbend}

\title{Puyeo's Power-bend discrete distribution
}

\description{
Density, distribution function, quantile function and random generation for discrete
version of Pueyo's power-bend distribution with parameters \code{s} and \code{omega}.
}

\usage{
dpowbend( x, s, omega = 0.01, oM = -log(omega), log=FALSE)
ppowbend ( q, s, omega = 0.01, oM = -log(omega), lower.tail=TRUE, log.p=FALSE)
qpowbend( p, s, omega = 0.01, oM = -log(omega), lower.tail= TRUE, log.p=FALSE)
rpowbend( n, s, omega)
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
  \item{n}{number of random values to return.}
  \item{s}{
    positive real s > 1; exponent of the power-bend distribution.
  }
  \item{omega}{
	positive real greater than 0; bending parameter of the distribution. 
	The current implementation only accepts omega smaller than 5.
  }
  \item{oM}{
  real number; alternative parametrization which is used for faster fitting on the 
  \code{\link{fitpowbend}} function. The current implementation only accepts oM
  greater than approximately -1.5. You can specify either 'omega' or 'oM', but not both.
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

  The bending term can be seen as a finite-size correction of the power law
  (Pueyo 2006). Therefore, the power-bend can describe better samples taken from power-law distributions.
  
  The function \eqn{Li_s(e^\omega)}{Li_s(exp(omega))} is the Polylogarithm of the 
  exponential of \code{omega} on base \code{s}, and represents the integration constant.
  A naive implementation of the Polylogarithm function is included in the package, and
  accepts values for non-integer \code{s} and \code{omega} smaller than 5, which cover the
  cases of biological relevance.

  This distribution was proposed by S. Pueyo to describe species
  abundance distributions (sads) as a generalization of the
  logseries distribution. Fisher's logseries correspond to the
  power-bend with parameters s=1 and
  \eqn{\omega = - \log(\frac{N}{N + \alpha})}{omega = -log(N/(N + alpha))}
  where N is sample size or total number of individuals and \eqn{alpha}{\alpha} is the
  Fisher's alpha parameter of the logseries.

  When fitted to sads, power-law distributions usually overestimates the abundance of common
  species, and in practice power-bend corrects this problem and usually
  provides a better fit to abundance distributions.
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
  Oikos 112: 392-405. 
}

\author{Paulo I Prado \email{prado@ib.usp.br} and Andre Chalom.}

\seealso{
  \code{dpower} for the power distribution; \code{\link{dls}} for the
  logseries distribution, which is a particular case of the power-bend,
  \code{\link{fitpowbend}} for maximum likelihood estimation in
  the context of species abundance distributions. 
}

\examples{
x <- 1:20
PDF <- dpowbend(x=x, s=2.1, omega=0.5)
CDF <- ppowbend(q=x, s=2.1, omega=0.5)
par(mfrow=c(1,2))
plot(x,CDF, ylab="Cumulative Probability", type="b",
     main="Powerbend distribution, CDF")
plot(x,PDF, ylab="Probability", type="h",
     main="Powerbend distribution, PDF")
par(mfrow=c(1,1))

## The powbend distribution is a discrete PDF, hence:
all.equal( ppowbend(10, s=2.1, omega=0.5), sum(dpowbend(1:10, s=2.1, omega=0.5)) ) # should be TRUE

## quantile is the inverse of CDF
all.equal(qpowbend(CDF, s=2.1, omega=0.5), x)

## Equivalence between power-bend and logseries
x <- 1:100
N <- 1000
alpha <- 5
X <- N/(N+alpha)
omega <- -log(X)
PDF1 <- dls(x, N, alpha)
PDF2 <- dpowbend(x, s=1, omega)
plot(PDF1,PDF2, log="xy")
abline(0,1, col="blue")
}
