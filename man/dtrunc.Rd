\name{dtrunc}

\alias{dtrunc}
\alias{ptrunc}
\alias{qtrunc}
\alias{rtrunc}

\title{Left- and right-truncation of density, probability and quantile of distributions}

\description{Returns density, probability, quantile values and random generation for
  distribution functions truncated at a specified minimum value (left
  truncation), a specified maximum value (right truncation), or both.}

\usage{
dtrunc(f, x, trunc, trunc.max, coef, log = FALSE)
ptrunc(f, q, trunc, trunc.max, coef, lower.tail=TRUE, log.p=FALSE)
qtrunc(f, p, trunc, trunc.max, coef, lower.tail = TRUE, log.p = FALSE)
rtrunc(f, n, trunc, trunc.max, coef, \dots)
}

\arguments{

  \item{f}{
    character;
    root name of the density or distribution function to be truncated
    - e.g., \kbd{"lnorm"} for the lognormal distribution; \kbd{"geom"} for the geometric distribution.
  }
  
  \item{x, q}{
    vector of quantiles.
  }
  
  \item{trunc}{
    numeric, \code{trunc < max(x)}. Lower truncation value, for
    left-truncation (see details). If missing, the distribution is
    not truncated on the left.
  }
  
  \item{trunc.max}{
    numeric, \code{trunc.max > min(x)}, and \code{trunc.max > trunc}
    if \code{trunc} is also provided. Upper truncation value, for
    right-truncation (see details). If missing, the distribution is
    not truncated on the right.
  }
  
  \item{p}{
    vector of probabilities.
  }

  \item{n}{
    number of random values to return.
  }
  
    \item{coef}{
      numeric named list; parameters values of the density or
      distribution function, named accordingly (see details). 
    }
  
  \item{log, log.p}{
    logical; if TRUE, probabilities p are given as log(p). 
  }
  
  \item{lower.tail}{
    logical; if TRUE (default), probabilities are P[X <= x],
    otherwise, P[X > x].
  }

  \item{\dots}{in \code{rtrunc} further arguments to
    be passed to \code{poweRlaw::rpldis} when argument argument
    \code{f = "power"}, \code{trunc.max} is missing, and argument
    \code{bissection = FALSE}. The same for
    \code{rpower} when and argument \code{bissection = FALSE}.}
}

\value{
  \code{dtrunc} gives the (log) density defined by \code{f}
  truncated at \code{trunc} and/or \code{trunc.max}.
  \code{ptrunc} gives the (log) distribution function defined by
  \code{f} truncated at \code{trunc} and/or \code{trunc.max}.
  \code{qtrunc} gives the quantile of the density defined by \code{f}
  truncated at \code{trunc} and/or \code{trunc.max}.
  \code{rtrunc} generates a sample from the density defined by
  \code{f} truncated at \code{trunc} and/or \code{trunc.max}.
}

\details{
  Given a distribution with probability distribution function (PDF) g
  and cumulative distribution function (CDF) G, a random variable
  \code{x} with this distribution truncated to the interval
  \code{(trunc, trunc.max]} has its PDF:

  g'(x) = g(x)/(G(trunc.max) - G(trunc)) for any trunc < x <= trunc.max, and zero otherwise

  and CDF:

  G'(x) = (G(min(max(x,trunc),trunc.max)) - G(trunc)) / (G(trunc.max) - G(trunc))

  If \code{trunc} is not provided, \code{G(trunc)} above is taken to
  be zero (no truncation on the left, i.e. left tail included down to
  the support of \code{f}). If \code{trunc.max} is not provided,
  \code{G(trunc.max)} above is taken to be one (no truncation on the
  right, i.e. right tail included up to the support of \code{f}). At
  least one of \code{trunc} or \code{trunc.max} must be given for
  truncation to be applied; if both are missing, \code{dtrunc},
  \code{ptrunc}, \code{qtrunc} and \code{rtrunc} return the same
  values as the underlying, non-truncated functions.

  \code{dtrunc} and \code{ptrunc} calculate the truncated
  distributions functions
  g'(x) and G'(x) defined above for
  a vector of values \code{x} from any
  standard distribution function available in R.
  This means the truncated tail(s) of a continuous distribution
  are dropped and the remaining density is rescaled to integrate to one.
  Accordingly, for discrete distributions, the probabilities
  for all \code{x} in \code{(trunc, trunc.max]} are rescaled to sum one.
  \code{qtrunc} is the inverse function of \code{ptrunc}.

  Left-, right- and doubly-truncated distributions can be used to
  describe the species abundance distributions (SADs),
  specially for continuous distributions
  (e.g., truncated lognormal distribution).
}

\references{
  Nadarajah, S. and Kotz, S. 2006. R Programs for Computing Truncated
  Distributions. \emph{Journal of Statistical Software 16}:Code Snippet 2.
}

\seealso{
  Distributions for standard distributions in R;
  many functions in package \pkg{sads} have an argument \code{trunc} that
  allows to simulate and fit truncated
  distributions
  for species abundance distributions (e.g., \code{\link{fitsad}}
  \code{\link{rsad}}, \code{\link{radpred}}, \code{\link{octavpred}}.
  Package 'VGAM' has truncated versions of many standard functions;
  see \code{Truncate-methods} in package \pkg{distr} for  general
  methods to build R objects of truncated distributions.
}

\source{
  Codes from Nadarajah and Kotz (2006),
  which provide a more generic solution
  for left and right truncation.
}

\examples{
A <- dtrunc("lnorm", x = 1:5, trunc = 0.5,
       coef = list( meanlog=1, sdlog=1.5 ) )
## same as
B <- dlnorm( 1:5 , meanlog = 1, sdlog = 1.5 ) /
  ( plnorm ( 0.5 , meanlog = 1, sdlog = 1.5, lower = FALSE))
## checking
identical( A, B )

## Right-truncation (truncation at a maximum)
A <- dtrunc("lnorm", x = 1:5, trunc.max = 3,
       coef = list( meanlog=1, sdlog=1.5 ) )
## same as
B <- dlnorm( 1:5 , meanlog = 1, sdlog = 1.5 ) /
  ( plnorm ( 3 , meanlog = 1, sdlog = 1.5 ) )
B[ 1:5 > 3 ] <- 0
## checking
identical( A, B )

## Truncation at both a minimum and a maximum
dtrunc("lnorm", x = 1:5, trunc = 0.5, trunc.max = 3,
       coef = list( meanlog=1, sdlog=1.5 ) )

A <- ptrunc("pois", q = 1:5, trunc = 0,
       coef = list( lambda = 1.5 ) )
## same as
B <- (ppois( 1:5 , lambda = 1.5 ) -
      ppois(0 , lambda = 1.5 ) ) /
  (ppois(0 , lambda = 1.5, lower = FALSE))
## checking
identical(A,B)

# Random generation
rtrunc("ls", 100, coef=list(N=1000, alpha=50), trunc=5)

# Random generation, truncated at a maximum
rtrunc("lnorm", 100, coef=list(meanlog=1, sdlog=1.5), trunc.max=3)
}


