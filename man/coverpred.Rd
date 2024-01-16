\name{coverpred-methods}
\docType{methods}
\alias{coverpred}
\alias{coverpred-methods}
\alias{coverpred,fitsadC,missing,missing,missing,missing,missing,missing-method}
\alias{coverpred,histogram,character,list,ANY,missing,missing,missing-method}
\alias{coverpred,missing,character,list,ANY,numeric,ANY,numeric-method}

\title{ ~~ Methods for Function \code{coverpred} in Package \pkg{sads} ~~}

\description{Creates an object of \code{coverpred-class} with the
  number of species in each abundance class predicted by a species
  abundance distribution.}

\section{Methods}{
  \describe{
    \item{\code{signature(object = "fitsadC", sad = "missing",
	coef = "missing", trunc = "missing", breaks = "missing",
	mids = "missing", S = "missing")}}{
      number of species in each abundance class
      predicted from a sads model fitted with function
      \code{fitsadC}.
    }

    \item{\code{signature(object = "histogram", sad = "character",
	coef = "list", trunc = "ANY", breaks = "missing",
	mids = "missing", S = "missing")}}{
      number of species in each abundance class
      predicted from abundance distribution named by \code{sad} with
      parameters defined in \code{coef}. Number of species S and
      intervals of the abundance classes defined are given by
      \code{histogram}.
    }
    
    \item{\code{signature(object = "missing", sad = "character",
	coef = "list", trunc = "ANY", breaks = "numeric",
	mids = "ANY", S = "numeric")}}{
      number of species each abundance class
      predicted from abundance distribution named by \code{sad} with
      parameters defined in \code{coef},  number of species S defined
      in \code{S}. Abundance classes are defined by their breakpoints
      (\code{breaks}) or by their midpoints (\code{mids}).
    }
    
}}

\arguments{
  
  \item{object}{an object of class \code{fitsadC} or \code{histogram};
    fitted model of species abundances
    distributions. Alternatively a histogram with number of species in
    each abundance class. 
  }
  \item{sad}{character; root name of sad distribution to
    calculate expected percentiles. See \code{\link{fitsadC}}
    for available distributions.
  }
  \item{coef}{named list of numeric values; parameter values of the
    distribution given in \code{sad}. Parameters should be named as in the corresponding density
    function, and in the same order.
  }
  \item{trunc}{non-negative integer, trunc > min(x); truncation point
    if fitted distribution is truncated.
  }
  \item{breaks}{real vector; breakpoints of abundance classes.
  }
  \item{mids}{real vector; breakpoints of abundance classes.
  }
  \item{S}{integer; total number of species.
  }
}


\keyword{methods}



\author{
  Paulo I. Prado \email{prado@ib.usp.br}
}

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
## Predicted values for the same data but other parameter values
grass.p.pred2 <- coverpred(grass.h, sad = "pareto", coef = list(shape = 1, scale = 0.5)) 
## Adds the new predicted values to the plot
points(grass.p.pred2, col = "red") 
}
