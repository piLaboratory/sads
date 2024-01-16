\name{coverpred-class}

\docType{class}

\alias{coverpred-class}
\alias{points,coverpred-method}

\title{Class \code{"coverpred"} for predicted values for abundance classes}

\description{ A list with values that define abundance classes and
  number and densities of species in each abundance, as predicted by a
  model of species abundance distribution.
}

\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("coverpred", \dots)},
  but most often by a call to \code{\link{coverpred}}.
}


\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{"list"} with five
  vectors: breaks of abundance classes, midpoints of abundance classes,
  upper limit of abundance classes, predicted proportion of species in each
  abundance class, predicted number of species in each abundance class.}}
}

\section{Extends}{
  Class \code{"\linkS4class{list}"}, directly.
  Class \code{"\linkS4class{oldClass}"}, by class "list", distance 2.
  Class \code{"\linkS4class{vector}"}, by class "list", distance 3.
}

\section{Methods}{
  \describe{
    \item{points}{\code{signature(x = "coverpred")}: adds frequency (or
  density) data contained in the object as points in a histogram. }
  }
}

\usage{
\S4method{points}{coverpred}(x, y.scale = c("density", "Freq", "prob"), mid = TRUE, \dots)
}

\arguments{
  \item{x}{an object of class \code{coverpred}}
  \item{y.scale}{"density" plots points in density scale, "Freq" plots
    frequencies and "prob" plots relative frequencies.}
  \item{mid}{logical; if \kbd{TRUE} x coordinates of abundances are set
  to the mid of each class, if \kbd{FALSE} x coordinates of abundances
  are the values of class upper limit.}
  \item{\dots}{further parameters to be passed to \code{lines}.}
}


\author{Andre Chalom and Paulo I Prado \email{prado@ib.usp.br}}


\seealso{ \code{\link{coverpred}} to get an object of the class from a
  histogran, \code{\link{fitsadC}} for fitting species abundance
  distributions to abundance data aggregated in classes.}

\examples{
## Example of fitting a sad model to cover data 
## Abundance classes: cover scale for plants
Lbrk <- c(0,1,3,5,15,25,35,45,55,65,75,85,95,100)
## To fit a sad model to cover data, data sould be in histogram format
grass.h <- hist(grasslands$mids, breaks = Lbrk, plot = FALSE)
## Fits a Pareto distribution to the histogram object
grass.p <- fitparetoC(grass.h)
## Values (densities, frequencies, relative frenquecies) predicted by the model for each size class
grass.p.pred <- coverpred(grass.p)
## Plot histogram of observed values in density scale
plot(grass.h)
## adds points for the predicted values (predicted densities)
points(grass.p.pred)
}

\keyword{classes}
