\name{dmand}
\alias{dmand}
\alias{pmand}
\alias{qmand}
\title{Zipf-Mandelbrodt distribution}

\description{
Density, distribution function and quantile function for
Zipf-Mandelbrodt distribution with parameters \code{N} \code{s} and \code{v}.
}

\usage{
dmand( x, N, s, v, log=FALSE)
pmand( q, N, s, v, lower.tail=TRUE, log.p=FALSE)
qmand( p, N, s, v, lower.tail = TRUE, log.p = FALSE)
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
    positive real s > 0; Zipf-Mandelbrodt exponent.
  }
  \item{v}{
    positive real v > 0; Zipf-Mandelbrodt parameter.  
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
  The Mandelbrodt distribution describes the probability or frequency of occurrence
  of a given element from a set of \code{N} elements. This probability  is inversely proportional to a power \code{s} of the
  rank of the frequency of the element in the set. The density function is

  p(x) = ((x+v)^(-s)) / sum(((1:N)+q)^(-s))

  Since p(x) is proportional to a power of \code{x}, the Zipf-Mandelbrodt distribution is a
  power distribution. The Zipf distribution is a special case when
  v=0. Hence, the Zipf-Mandelbrodt distribution is generalization of the
  Zipf Law, and is widely used in the same cases. In Ecology, it
  can be used to describe the probability of the abundance rank \code{x} of given species in a sample or assemblage of \code{N}
  species.
  
}

\value{
  
\code{dmand} gives the (log) density of the density, \code{pmand} gives the (log)
distribution function, \code{qmand} gives the quantile function.

Invalid values for parameters \code{v} or \code{s} will result in return
values \code{NaN}, with a warning.
}

\references{

  Johnson N. L., Kemp, A. W. and Kotz S. (2005) Univariate Discrete
  Distributions, 3rd edition, Hoboken, New Jersey: Wiley. Section
  11.2.20. 

  Magurran, A. E., and McGill, B. J. (2011). Biological Diversity -
  Frontiers in measurement and assessment. Oxford: Oxford University Press.
  
}

\author{
Paulo I. Prado and Murilo Dantas Miranda
}

\seealso{
\code{dmand} and \code{rmand} and related functions in \pkg{mandR} package; \code{Zeta} for
zeta distribution in \pkg{VGAM} package.
}

\examples{
x <- 1:100
pdf <- dmand(x=x, N=100, s=1.5, v=2)
cdf <- pmand(q=x, N=100, s=1.5, v=2)
par(mfrow=c(1,2))
plot(x,cdf, ylab="Cumulative Probability", type="b",
     main="Zipf-Mandelbrodt distribution, CDF")
plot(x,pdf, ylab="Probability", type="h",
     main="Zipf-Mandelbrodt distribution, PDF")
par(mfrow=c(1,1))

## quantile is the inverse of cdf
sum(qmand(cdf,N=100, s=1.5, v=2)-x) # should be zero

## Zipf distribution is a particular case of ZM when s=0
sum(dmand(x=x, N=100, s=1.5, v=0)-dzipf(x=x, N=100, s=1.5)) # should be zero
}