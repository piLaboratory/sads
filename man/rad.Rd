\name{rad}

\alias{rad}

\title{Rank-abundance data frame}

\description{Creates a data frame with ranked abundances of \code{rad-class} from a vector
  of abundances or fitted model object.
}

\usage{rad(x)}

\arguments{
  \item{x}{a numerical vector of abundances or an object of class
    \code{fitsad} or \code{fitrad}.}
}

\value{
  an object of \code{\link{rad-class}}, which is a data frame with two vectors:
  \item{rank}{integer; abundance rank of each species (integer), from most abundant
    (rank=1) to the least abundant (rank=length(rank))}
  \item{abund}{numeric; abundance of each species}
  
}


\references{
  Whittaker, R. H. 1965, Dominance and Diversity in Land Plant
  Communities. \emph{Science, 147}: 250--260.
}

\seealso{ \code{\link{rad-class}} for methods to create a rank-abundance
  data frame; \code{\link{radpred}} to get a \code{rad-class} object of predicted abundances from a
  theoretical distribution;  \code{\link{fitsad-class}} and
  \code{\link{fitrad-class}} objects, from which you can also get an
  object of class \code{rad}; \code{\link{qqrad}} for quantile-quantile plots from a
  \code{rad-class} object, and \code{\link{pprad}} for
  percentile-percentile plots. 
}

\examples{

## rank-abundance plot
plot(rad(okland))

}

\author{
  Paulo I. Prado \email{prado@ib.usp.br} and Murilo Dantas Miranda.
}
