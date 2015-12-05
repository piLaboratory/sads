\name{octav-class}

\docType{class}

\alias{octav-class}
\alias{lines,octav-method}
\alias{plot,octav,ANY-method}
\alias{plot,octav-method}
\alias{points,octav-method}

\title{Class \code{"octav"} for frequencies in abundance octaves}

\description{ Data frame of frequencies of entities (usually species) in classes
  of logarithm of abundances at base 2 (Preston's octaves).
}

\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("octav", \dots)},
  but most often by a call to \code{\link{octav}} or \code{\link{octavpred}.}
}

\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{"list"} a data frame of
      three vectors: octave number (integer), which is the upper
      limit of the class in log2; upper limit of the class in arithmetic
      scale (numeric); number of cases in each class (numeric). }
    \item{\code{names}:}{Object of class \code{"character"}; names of
      the three vectors of \code{.Data}, \code{"octave"},
      \code{"upper"}, and \code{"Freq"}, respectively.  }
    \item{\code{row.names}:}{Object of class
      \code{"data.frameRowLabels"}; default line names for \code{.Data}. }
    \item{\code{.S3Class}:}{Object of class \code{"character"};
      indicates inheritance from S3 class \code{data.frame}. }
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
    \item{lines}{\code{signature(x = "octav")}: adds frequency data
      contained in the object as lines in an octave plot created by \code{plot} method.}
    \item{plot}{\code{signature(x = "octav", y = "ANY")}: creates a
      histogram of frequencies of species in each octave in the object, a.k.a 'Preston plot' }
    \item{points}{\code{signature(x = "octav")}: adds frequency data
      contained in the object as points in a octave plot created by \code{plot} method. }
  }
}

\usage{
\S4method{lines}{octav}(x, prop=FALSE, mid=TRUE, \dots)
\S4method{plot}{octav}(x, prop=FALSE, x.oct=FALSE, par.axis=list(), \dots)
\S4method{points}{octav}(x, prop=FALSE, mid=TRUE, \dots)
}

\arguments{
  \item{x}{an object of class \code{octav}}
  \item{prop}{logical; if \kbd{TRUE} relative frequencies are returned.}
  \item{x.oct}{logical; if \kbd{TRUE} axis labels are octave numbers, if
    \kbd{FALSE} upper limit of abundance class are used as labels.}
  \item{mid}{logical; if \kbd{TRUE} x coordinates of abundances are set
  to the mid of each octave, if \kbd{FALSE} x coordinates of abundances
  are the values of its octave (see examples)}
  \item{par.axis}{list; further graphical parameters for the plot axes.}
  \item{\dots}{further parameters to be passed to \code{lines}, \code{points} or \code{plot}
    functions (except axes in \code{plot}, which are set by \code{par.axis}.}
}

\references{
  Magurran, A.E. 1989. \emph{Ecological diversity and its measurement}.
  Princenton University Press.
  
  Preston, F.W. 1948. The commonness and rarity of species.
  \emph{Ecology 29}: 254--283.
}

\author{Andre Chalom and Paulo I Prado \email{prado@ib.usp.br}}


\seealso{ \code{\link{octav}} to get an object of the class from a vector
  of abundances; \code{\link{octavpred}} to get an \code{octav} object of
  predicted abundances from a theoretical distribution;
  man page of \code{prestonfit} in package \code{vegan} for a detailed account of
  Preston's octaves and an alternative way to get octaves and model fitting.
}

\examples{
## Creates an octav object from an abundance vector
birds.oc <- octav(birds)
moths.oc <- octav(moths)

## default plot
plot(birds.oc)
## Octave values instead of abundances at x-axis
plot(moths.oc, x.oct=TRUE)

## Using line and argument prop to superpose two data sets
## (Fisher's et al moth data and Preston's bird data)
## mid=FALSE plots points at each octave value and not
## in the midclass (default, useful for histograms)
plot(moths.oc, col=NULL, border=NA, prop=TRUE, x.oct=TRUE, xlab="Octave")
lines(moths.oc, prop=TRUE,  mid=FALSE)
lines(birds.oc, prop=TRUE, mid=FALSE, col="red") 

}

\keyword{classes}
