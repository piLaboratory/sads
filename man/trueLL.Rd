\name{trueLL}

\alias{trueLL}

\title{True likelihood for continuous variables}

\description{
  Calculates the correct likelihood for independent observations of a
  continuous variable that follows a given (truncated) density function.
}

\usage{
trueLL(x, dens, coef, trunc, dec.places = 0, log = TRUE, \dots)
}

\arguments{
  \item{x}{
    vector of observed values.
  }
  \item{dens}{
    character; root name of the continuous density distribution of the variable -
    e.g., \kbd{lnorm} for the lognormal distribution; \kbd{gamma} for the gamma
    distribution.
  }
  \item{coef}{named list; values of the coefficients of the continuous
    density distribution, in the same order and with the same names as in
    the probability function defined by the argument \code{dens}.
  }

  \item{trunc}{
    real number \code{trunc <= min(x)}; value from which the density distribution is
    truncated. Currently only lower-tail truncation (or right-truncation)
    is implemented. If this argument is omitted the whole
    distribution is used (default).
  }

  \item{dec.places}{
    positive integer; number of decimal places used in the measurement of
    the observed values. Observed values will be rounded to this number of
    decimals. This argument defines the measurement precision, see details. 
  }

  \item{log}{
    logical; convert probabilities to logs and sum them to get
    log-likelihoods? The log-likelihood value is returned if \kbd{TRUE} and the likelihood
    value if \kbd{FALSE} (quickly tends to zero as number of observation increases).
  }
  \item{\dots}{
    named arguments to be passed to the probability function defined by the argument \code{dens}.
  }
}
\details{
  The (log)likelihood function is often defined as any function
  proportional to the (sum) product of (log) probability density of the
  observations. In its original formulation, however, the likelihood is
  proportional to the product of probabilities, not probabilities
  densities (Fisher 1922, stressed by Lindsey 1999).  For continuous
  variables these probabilities are calculated through integration of
  the probability density function in an interval. For observed values
  of continuous variables, this interval is defined by the measurement
  precision.  The likelihood function is thus any function proportional
  to
  
  prod ( integral_(x-D)^(x+D) f(x) dx )

  where x is the observed value, f(x) the
  density function and D is half the measurement precision:

  D = 10^(-'dec.places')/2
  
  Because products of probabilities quickly tends to
  zero, probabilities are usually converted to their logs and then
  summed to give the log-likelihood function.
}

\value{
  (log)-likelihood for the observations, under the (truncated) probability density
  model defined by the argument \code{dens} and their parameters, passed
  through the \code{\dots} argument.
}
\references{
  Fisher, R.A. 1922. On the mathematical foundations of theoretical
  statistics. \emph{Phil. Trans. R. Soc. Lond. A 222}: 309--368.
  
  Lindsey, J.K. 1999. Some statistical heresies. \emph{The Statistician
    48}(1): 1--40.
}
\author{
  Paulo I. Prado \email{prado@ib.usp.br}
}


\seealso{
  \code{logLik} in the package \pkg{MASS} and \code{logLik-methods} in
  package \pkg{bbmle}.
}

\examples{
##Random sample of a lognormal distribution with precision=0.1
x <- round(rlnorm(500,meanlog=3,sdlog=0.5),1)
## Log-likelihood given by fitdistr
library(MASS)
logLik(fitdistr(x,"lognormal"))
## Which is the sum of log of densities
sum( dlnorm(x, meanlog=mean(log(x)), sdlog=sd(log(x)), log=TRUE) )
## Correct log-likelihood
trueLL(x, "lnorm", coef=list(meanlog=mean(log(x)), sdlog=sd(log(x))), dec.places=1, )
}
