\name{dsmsl}
\alias{dsmsl}
\alias{psmsl}
\alias{qsmsl}
\title{Dewdney's Multispecies Logistical (MSL) distribution}

\description{
  Density, distribution and quantile function
  for Dewdney's special MSL distribution with parameters \code{delta} and \code{epsilon}.
}

\usage{
dsmsl(x, delta, epsilon = min(x), log = FALSE)
psmsl(q, delta, epsilon, lower.tail=TRUE, log.p=FALSE)
qsmsl(p, delta, epsilon, lower.tail = TRUE, log.p = FALSE)
}

\arguments{
  \item{x}{
    vector of (non-negative integer) quantiles. In the context of
    species abundance distributions, this is a
    vector of abundance of species in a sample.
  }
  \item{q}{vector of (non-negative integer) quantiles. In the context of
    species abundance distributions, a vector of abundance of species in a sample.
  }
   \item{p}{
     vector of probabilities.
   }
  \item{delta}{
    positive real > 0; Represents the lowercase "delta" parameter in Dewdney's
    special MSL model. It is the the inverse of the non-zero extent of the distribution 
    function (see details for the formula).
  }
  \item{epsilon}{
   positive real > 0; the epsilon parameter. It is related to the inverse of the predicted
   number of species in the lowest abundance category. The default value for this parameter 
   in \code{dsmsl} is the minimum value of the abundance vector, as it is the value that
   always gives the maximum likelihood.
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
  The multispecies logistical system has been proposed by A.K. Dewdney in a series of papers
  from 1997 to 2003 (Dewdney 2003), as a proposed improvement to the Preston lognormal model.
  Dewdney's papers present two strongly related formulas for the "special" and the "general"
  logistic J-distribution; the special case arises from a formal demonstration from MSL system.

  The probability function for the sMSL distribution is:

  \deqn{f(x) = c \left(\frac{1}{x} - \delta\right); \epsilon \leq x \leq \frac{1}{\delta}}
       {f(x) = c (1/x - delta); epsilon <= x <= 1/delta}

  And zero otherwise. Notice that the distribution is undefined if epsilon > 1/delta.
  
  The parameters to this distribution are delta and epsilon, and
  c is a constant of integration, which can be deduced as:

  \deqn{\frac{1}{c} = \delta \epsilon - \log(\delta \epsilon) - 1}
       {1/c = delta * epsilon - log(delta * epsilon) - 1}

  The cumulative distribution function is found by the algebraic integration of the density
  function above; however, the quantile function is given by a numeric approximation.

  It should be noted that, due to the definition of the density function above, the 
  procedures normally used for truncation of the distribution (see \code{\link{dtrunc}}
  are meaningless.
}

\value{
  
\code{dsmsl} gives the (log) density function, \code{psmsl} gives the (log)
distribution function, and \code{qsmsl} gives the quantile function.

Invalid values for parameters \code{delta} or \code{epsilon} will result in return
values \code{NaN}, with a warning.
}

\references{

  Dewdney, A.K. 2003. The stochastic community and the logistic-J distribution.
  \emph{Acta Oecologica 24}: 221-229.
}

\author{Andre Chalom and Paulo Prado}


\examples{
  x <- 1:100
  PDF <- dsmsl(x=x, delta = 1/100, epsilon=0.25)
  CDF <- psmsl(q=x, delta = 1/100, epsilon=0.25)
  par(mfrow=c(1,2))
  plot(x,CDF, ylab="Cumulative Probability", type="b",
     main="Dewdney's MSL distribution, CDF")
  plot(x,PDF, ylab="Probability", type="h",
     main="Dewdney's MSL distribution, PDF")
  par(mfrow=c(1,1))
  ## quantile is the inverse of CDF
  all.equal( qsmsl( CDF, delta=1/100, epsilon=0.25), x) # should be TRUE
}
