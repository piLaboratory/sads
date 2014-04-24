\name{dtrunc}
\alias{dtrunc}
\alias{ptrunc}
\title{Left-truncation of density and probability distributions}
\description{Returns density and probability values for
  distribution functions left-truncated at a specified value.}
\usage{
dtrunc(f, x, trunc, coef, log = FALSE)
ptrunc(f, q, trunc, coef, lower.tail=TRUE, log.p=FALSE)
}
\arguments{

  \item{f}{
    character;
    root name of the density or distribution function to be truncated
    - e.g., "lnorm" for the lognormal distribution; "geom" for the geometric distribution.
  }
  
  \item{x, q}{
    vector of quantiles.
  }
  
  \item{trunc}{
    numeric, trunc>min(x). Truncation value (see details).
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
}

\value{
  \code{dtrunc} gives the (log) density defined by \code{f} left-truncated at \code{trunc}.
  \code{ptrunc} gives the (log) distribution function defined by
  \code{f} left-truncated at \code{trunc}.
}

\details{
  Given a distribution with probability distribution function (PDF) g
  and cumulative distribution function (CDF) G, a random variable
  \code{x} with these distributions left-truncated at \code{trunc} has
  its PDF:

  g'(x) = g(x)/(1 - G(trunc)) for any x <= trunc and zero otherwise 

  and CDF:

  G'(x) = (G(max(x,trunc)) - G(trunc)) / (1 - G(trunc))

  \code{dtrunc} and \code{ptrunc} calculates the left-truncated
  distributions functions
  g'(x) and G'(x) defined above for
  a vector of values \code{x} from any
  standard distribution function available in R.
  This means that the "upper tail" of a continuous distribution
  is rescaled to integrate to one.
  Accordingly, for discrete distributions, the probabilities
  for all \code{x}>trunc are rescaled to sum one.

  Left-truncated distributions can be used to
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
  many functions in package 'sads' have an argument \code{trunc} that
  allows to simulate and fit truncated
  distributions
  for species abundance distributions (e.g., \link{fitsad}
  \link{rsad}, \link{radpred}, \link{octavpred}.
  Package 'VGAM' has truncated versions of many standard functions;
  see \code{Truncate-methods} in package 'distr'for  general
  methods to build R objects of truncated distributions.
}

\source{
  Codes from Nadarajah and Kotz (2006),
  which is a more generic solution
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

A <- ptrunc("pois", q = 1:5, trunc = 0,
       coef = list( lambda = 1.5 ) )
## same as
B <- (ppois( 1:5 , lambda = 1.5 ) -
      ppois(0 , lambda = 1.5 ) ) /
  (ppois(0 , lambda = 1.5, lower = FALSE))
## checking
identical(A,B)
}


