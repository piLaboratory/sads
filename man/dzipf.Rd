\name{dzipf}
\alias{dzipf}
\alias{pzipf}
\alias{qzipf}

\title{Zipf distribution}

\description{
  Density, distribution function and quantile function for
  Zipf distribution with parameters \code{N} and \code{s}.
}

\usage{
dzipf( x, N, s, log=FALSE)
pzipf( q, N, s, lower.tail=TRUE, log.p=FALSE)
qzipf( p, N, s, lower.tail = TRUE, log.p = FALSE)
}

\arguments{
  \item{x}{
    vector of (non-negative integer) quantiles. In the context of
    species abundance distributions, this is a vector of abundance ranks of species in a sample.
  }
  \item{q}{vector of (non-negative integer) quantiles. In the context of
    species abundance distributions, a vector of abundance ranks of species in a sample.
  }
  \item{p}{
    vector of probabilities.
  }
  \item{N}{
    positive integer 0 < N < Inf, total number of elements of a collection. In the context of
    species abundance distributions, usually the number of species in a sample.
  }
  \item{s}{
    positive real s > 0; Zipf's exponent
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
  The Zipf distribution describes the probability or frequency of occurrence
  of a given element from a set of \code{N} elements. According to Zipf's
  law, this probability  is inversely proportional to a power \code{s} of the frequency
  rank of the element in the set. The density function is

  p(x) = (x^(-s)) / sum((1:N)^(-s))

  Since p(x) is proportional to a power of \code{x}, the Zipf distribution is a
  power distribution. The Zeta distribution is a special case at the limit
  N -> Inf.

  The Zipf distribution has a wide range of applications (Li 2011). One
  of its best know application is to describe the probability
  of occurrence of a given word that has a ranking \code{x} in a \emph{corpus} that has a total of \code{N}
  words. It can be used as well to describe the probability of the
  abundance rank of a given species in a sample or assemblage of \code{N} species.
}

\value{
  \code{dzipf} gives the (log) density, \code{pzipf} gives the (log)
  distribution function, \code{qzipf} gives the quantile function.
}

\references{

  Johnson N. L., Kemp, A. W. and Kotz S. (2005) \emph{Univariate Discrete
  Distributions}, 3rd edition, Hoboken, New Jersey: Wiley. Section
  11.2.20. 

  Li, W. (2011). Zipf's Law
  Bibliography. \url{http://www.nslij-genetics.org/wli/zipf/index.html}.
  
  Zipf's Law. \url{http://en.wikipedia.org/wiki/Zipf's_law}.
}

\author{Paulo I Prado \email{prado@ib.usp.br} and Murilo Dantas Miranda.}

\seealso{
  \code{dzipf} and \code{rzipf} and related functions in \pkg{zipfR} package; \code{Zeta} for
  zeta distribution in \pkg{VGAM} package. \code{\link{fitzipf}} to fit
  Zipf distribution as a rank-abundance model.
}

\examples{
x <- 1:20
PDF <- dzipf(x=x, N=100, s=2)
CDF <- pzipf(q=x, N=100, s=2)
par(mfrow=c(1,2))
plot(x,CDF, ylab="Cumulative Probability", type="b",
     main="Zipf distribution, CDF")
plot(x,PDF, ylab="Probability", type="h",
     main="Zipf distribution, PDF")
par(mfrow=c(1,1))

## quantile is the inverse of CDF
all.equal( qzipf(CDF, N=100, s=2), x) # should be TRUE

## Zipf distribution is discrete hence
all.equal( sum(dzipf(1:10, N=10, s=2)), pzipf(10, N=10, s=2)) # should be TRUE
}
