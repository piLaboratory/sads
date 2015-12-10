\name{dgs}

\alias{dgs}
\alias{pgs}
\alias{qgs}
\alias{rgs}

\title{Geometric series distribution}

\description{
Density, distribution function, quantile function and random generation for
the Geometric Series distribution, with parameter \code{k}.
}

\usage{
dgs( x, k, S, log = FALSE )
pgs( q, k, S, lower.tail = TRUE, log.p = FALSE )
qgs( p, k, S, lower.tail = TRUE, log.p = FALSE )
rgs( n, k, S )
}

\arguments{
  \item{x}{
    vector of (non-negative integer) quantiles. In the context of
    species abundance distributions, this is a vector of abundance ranks
    of species in a sample.
  }
  \item{n}{number of random values to return.}
  \item{k}{
    positive real, 0 < k < 1; geometric series coefficient; the ratio between
    the abundances of i-th and (i+1)-th species. 
  }
  \item{q}{vector of (non-negative integer) quantiles. In the context of
    species abundance distributions, a vector 
    of abundance ranks of species in a sample.
  }
  \item{p}{
    vector of probabilities.
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
  The Geometric series distribution gives the probability (or expected
  proportion of occurrences) of the i-th most abundant element
  in a collection:
  
  \deqn{p(i) = C k (1-k)^{i-1}}{p(i) = C * k * (1-k)^(i-1)}
  
  where C is a normalization constant which makes the summation of p(i) over
  S equals to one:
  
  \deqn{C = \frac{1}{1 - (1-k)^S}}{C = 1/(1 - (1-k)^S)}
  
  where S is the number of species in the sample.
  
  Therefore, \code{[dpq]gs} can be used as rank-abundance model
  for species ranks in a sample or biological community
  see \code{\link{fitrad-class}}.
}

\value{
  \code{dgs}  gives the (log) density and  \code{pgs} gives the (log)
  distribution function of ranks,  and \code{qgs} gives the
  corresponding quantile function.
}

\references{
  
  Doi, H. and Mori, T. 2012. The discovery of species-abundance
  distribution in an ecological community. \emph{Oikos 122:} 179--182.
  
  May, R.M. 1975. Patterns of Species Abundance and Diversity. In
  Cody, M.L. and Diamond, J.M. (Eds) \emph{Ecology and Evolution of
    Communities}. Harvard University Press. pp 81--120.
}

\note{
  The Geometric series is NOT the same as geometric distribution. In
  the context of community ecology, the first can be used
  as a rank-abundance model and the former as a species-abundance
  model. See \code{\link{fitsad}} and \code{\link{fitrad}} and vignettes
  of \pkg{sads} package.
}

\author{Paulo I Prado \email{prado@ib.usp.br} and Murilo Dantas Miranda.}


\seealso{ \code{\link{fitgs}}, \code{\link{fitrad}} to fit the Geometric series as a
  rank-abundance model.
  }

\examples{
x <- 1:25
PDF <- dgs(x=x, k=0.1, S=25)
CDF <- pgs(q=x, k=0.1, S=25)
par(mfrow=c(1,2))
plot(x,CDF, ylab="Cumulative Probability", type="b",
     main="Geometric series distribution, CDF")
plot(x,PDF, ylab="Probability, log-scale", type="h",
     main="Geometric series distribution, PDF", log="y")
par(mfrow=c(1,1))

## quantile is the inverse of CDF
all.equal(qgs(CDF, k=0.1, S=25), x)
}
