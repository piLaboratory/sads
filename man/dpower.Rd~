\name{dzipf}
\alias{dzipf}
\alias{pzipf}
\alias{qzipf}
\title{Zipf distribution
}
\description{
Density, distribution function and quantile function for
Zipf distribution with parameters ‘N’ and ‘s’.
}
\usage{
dzipf( x, N, s, log=FALSE)
pzipf( q, N, sig, lower.tail=TRUE, log.p=FALSE)
qzipf( p, N, sig, S=30)
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
    vetor of probabilities.
  }
  \item{N}{
    positive integer 0 < N <Inf, total number of elements of a collection. In the context of
    species abundance distributions, usually the number of species in a sample.
  }
  \item{s}{
  positive real s > 1; Zipf's exponent}
  \item{log, log.p}{
    logical; if TRUE, probabilities p are given as log(p).
  }
  \item{lower.tail}{
    logical; if TRUE (default), probabilities are P[X <= x],
    otherwise, P[X > x].
  }
}

\details{
  Zipf distribution describe the probability or frequency of occurrence
  of a given element from a set of 'N' elements. According to Zipf's
  law, this probability  is inversely proportional to a power 's' of the
  rank  of the element. The density function is

  p(x) = (x^(-s)) / sum((1:N)^(-s))

  Since p(x) is proportional to a power of 'x', the Zipf distribution is a
  power distribution. The Zeta distribution is a special case when N=Inf.

  The Zipf distribution is widely used to describe the probability
  of occurrence of a given word that has a ranking 'x' in a \emph{corpus} that has a total of 'N'
  words. It can be used as well to describe the probability of the rank
  or abundance 'x' of given species in a sample or assemblage of 'N' species.
}

\value{
'dzipf' gives the (log) density of the density, 'pzipf' gives the (log)
distribution function, 'qzipf' gives the quantile function.
}

\references{
  Zipf's Law. http://en.wikipedia.org/wiki/Zipf's_law.

  Johnson N. L., Kemp, A. W. and Kotz S. (2005) Univariate Discrete
  Distributions, 3rd edition, Hoboken, New Jersey: Wiley. Section 11.2.20. 
}

\author{
Paulo I. Prado and Murilo Dantas Miranda
}

\seealso{
dpois, dlnorm; dzipf and rzipf in zipfR package; rsad for random
generation fitzipf for maximum likelihood estimation in the context of species abundance distributions.
}
