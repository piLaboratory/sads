\name{rad-class}

\docType{class}

\alias{rad-class}
\alias{lines,rad-method}
\alias{plot,rad-method}
\alias{plot,rad,ANY-method}
\alias{points,rad-method}
%\alias{qqrad,rad-method}
%\alias{qqrad,rad,character,list,ANY-method}
%\alias{qqrad,rad,character,list,ANY,character-method}
%\alias{pprad,rad,character,list-method}
%\alias{qqrad,rad,character,list,ANY-method}

\title{Class \code{"rad"} for rank-abundance data}
\description{Data frame of ranked abundances of species
}

\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("rad", ...)}, but
  most often by a call to \code{\link{rad}} or \code{\link{radpred}}.
}

\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{"list"}; a data frame of
      two vectors: abundance rank of each species (integer), from most abundant
      (rank=1) to the least abundant (rank=length(rank)); abundance of each
      species (numeric)}
    \item{\code{names}:}{Object of class \code{"character"}; names of
      the two vectors of \code{.Data}, \code{"rank"} and \code{"abund"}, respectively. }
    \item{\code{row.names}:}{Object of class
      \code{"data.frameRowLabels"}; default line names for \code{.Data};
      integer indexes from 1 to \code{nrow(.Data)}.}
    \item{\code{.S3Class}:}{Object of class \code{"character"};
      indicates inheritance from S3 class \code{data.frame}.}
  }
}

\section{Extends}{
  Class \code{"\linkS4class{data.frame}"}, directly.
  Class \code{"\linkS4class{list}"}, by class "data.frame", distance 2.
  Class \code{"\linkS4class{oldClass}"}, by class "data.frame", distance 2.
  Class \code{"\linkS4class{vector}"}, by class "data.frame", distance 3.
}

\section{Methods}{
  \describe{
    \item{lines}{\code{signature(x = "rad")}: adds rank-abundance data
      contained in the object as lines in rank-abundance plots created by
      \code{plot} method.}
    \item{plot}{\code{signature(x = "rad", y = "ANY")}: creates a
      rank-abundance plot from data in the object.}
    \item{points}{\code{signature(x = "rad")}: adds rank-abundance data
      contained in the object as points in rank-abundance plots created by
      \code{plot} method. }
    \item{pprad}{\code{signature(x = "rad", rad = "character", coef =
    	"list")}: percentile-percentile plot, see \code{\link{pprad}}. }
    \item{qqrad}{\code{signature(x = "rad", rad = "character", coef =
    	"list", trunc = "ANY")}:  quantile-quantile plot, see
      \code{\link{qqrad}}.}
  }
}

\usage{
\S4method{lines}{rad}(x, prop=FALSE, \dots)
\S4method{plot}{rad}(x, prop=FALSE, \dots)
\S4method{points}{rad}(x, prop=FALSE, \dots)
}

\arguments{
  \item{x}{an object of class \code{rad}}
  \item{prop}{logical; if \kbd{TRUE} relative abundances are returned.}
  \item{\dots}{further parameters to be passed to \code{lines}, \code{points} or \code{plot}
    functions.}
}

\references{
  Whittaker, R. H. 1965, Dominance and Diversity in Land Plant
  Communities. \emph{Science, 147}: 250--260. 
}

\author{
  Paulo I. Prado \email{prado@ib.usp.br} and Murilo Dantas Miranda
}

\seealso{\code{\link{rad}} to get an object of the class from a vector
  of abundances; \code{\link{radpred}} to get a \code{rad-class} object of predicted abundances from a
  theoretical distribution, \code{\link{qqrad}} for quantile-quantile plots from a
  \code{rad-class} object, and \code{\link{pprad}} for
  percentile-percentile plots.
}

\examples{
## Creates a rad object from a vector of abundances
birds.rad <- rad(birds)

## Rank-abundance plot
plot(birds.rad)

## Same, with non-default graphical parameters
plot(birds.rad, pch=19, xlab="Abundance rank of species")

## Adding points from another data set
points(rad(okland), pch=19)
}

\keyword{classes}
