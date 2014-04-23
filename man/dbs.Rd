\name{dbs}

\alias{dbs}
\alias{pbs}
\alias{qbs}
\alias{drbs}
\alias{prbs}
\alias{qrbs}

\title{MacArthur's Broken-stick distribution}

\description{
Density, distribution function and quantile function for
the Broken-stick distribution with parameters \code{N} and \code{S}.
}

\usage{
dbs( x, N, S, log = FALSE )
pbs( q, N, S, lower.tail = TRUE, log.p = FALSE )
qbs( p, N, S, lower.tail = TRUE, log.p = FALSE )
drbs( x, N, S, log = FALSE )
prbs( q, N, S, lower.tail = TRUE, log.p = FALSE )
qrbs( p, N, S, lower.tail = TRUE, log.p = FALSE )
}

\arguments{
  \item{x}{
    vector of (non-negative integer) quantiles. In the context of
    species abundance distributions, this is a vector of abundances (for
    \code{dbs} and \code{drbs}) or abundance ranks (for \code{drbs})
    of species in a sample.
  }
  \item{q}{vector of (non-negative integer) quantiles. In the context of
    species abundance distributions, a vector of abundances
    (for \code{dbs} and \code{drbs})
    or abundance ranks (for \code{drbs}) of species in a sample.
  }
  \item{p}{
    vector of probabilities.
  }
  \item{N}{
    positive integer 0 < N < Inf, sample size. In the context of
    species abundance distributions, the sum of abundances of individuals in a sample.
  }
  \item{S}{
    positive integer 0 < S < Inf, number of elements in a collection.
    In the context of species abundance distributions, the number of species in a sample.
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
  
  The Broken-stick distribution was proposed as a model for
  the expected abundance of elements in a collection:

  n(i) = N/S (sum(from k=i to S) 1/k)

  where n(i) is the abundance in the i-th most abundant element
  (MacArthur 1960, May 1975).
  Hence the probability (or expected proportion of occurences)
  in the i-th element is

  p(i) = n(i)/N = n(i) = (sum(from k=i to S) 1/k) / S

  \code{[dpq]rbs} stands for "rank-abundance Broken-stick" and return
  probabilities and quantiles based on the expression above, for p(i).
  Therefore, \code{[dpq]rbs} can be used as rank-abundance model
  for species ranks in a sample or biological community
  see \code{\link{fitrad-class}}.

  The probability density for a given abundance value in the Broken-stick model
  is given by 

  p(x) = (1 - x/N)^(S-2) (S - 1)/N

  Where x is the abundance of a given element in the collection (May 1975).
  \code{[dpq]bs} return probabilities and quantiles according to the
  expression above for p(x).
  Therefore, \code{[dpq]bs} can be used as
  species abundance model
  see \code{\link{fitsad-class}}.
  }

\value{
  \code{dmand} gives the (log) density of the density, \code{pmand} gives the (log)
  distribution function, \code{qmand} gives the quantile function.
}

\references{

  MacArthur, R.H. 1960. On the relative abundance of species.
  \emph{Am Nat 94}:25--36.
  
  May, R.M. 1975. Patterns of Species Abundance and Diversity. In
  Cody, M.L. and Diamond, J.M. (Eds) \emph{Ecology and Evolution of
  Communities}. Harvard University Press. pp 81--120.
  }

\author{
Paulo I. Prado and Murilo Dantas Miranda
}

\seealso{
  \code{fitbs} and \code{fitrbs} to fit the Broken-stick distribution
  as a sad and rad model of species abundances.
}

\examples{
x <- 1:25
pdf <- drbs(x=x, N=100, S=25)
cdf <- prbs(q=x, N=100, S=25)
par(mfrow=c(1,2))
plot(x,cdf, ylab="Cumulative Probability", type="b",
     main="Broken-stick distribution, CDF")
plot(x,pdf, ylab="Probability", type="h",
     main="Broken-stick distribution, PDF")
par(mfrow=c(1,1))

## quantile is the inverse of cdf
sum(qrbs(cdf,N=100, S=25)-x) # should be zero
}
