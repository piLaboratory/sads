\name{octavpred-methods}

\docType{methods}

\alias{octavpred}
%\alias{octavpred-methods}
\alias{octavpred,fitrad,missing,missing,missing,missing,ANY,missing,missing-method}
\alias{octavpred,fitsad,missing,missing,missing,missing,ANY,missing,missing-method}
\alias{octavpred,missing,character,missing,list,ANY,ANY,numeric,numeric-method}
\alias{octavpred,missing,missing,character,list,ANY,ANY,numeric,numeric-method}
\alias{octavpred,numeric,character,missing,list,ANY,ANY,missing,missing-method}
\alias{octavpred,numeric,missing,character,list,ANY,ANY,missing,missing-method}

\title{Predicted frequencies of species in octaves}

\description{
  Creates an object of \code{octav-class} with the frequencies of
  species in octaves of abundances predicted by a species abundance
  distribution or by a rank-abundance distribution.
}

\section{Methods}{
  \describe{
    \item{\code{signature(object = "fitrad", sad = "missing", rad =
	"missing", coef = "missing", trunc = "missing", oct = "ANY", S =
	"missing", N = "missing")}}{number of species in each octave
      predicted from a rank-abundance model fitted with function
      \code{fitrad}.
    }
    \item{\code{signature(object = "fitsad", sad = "missing", rad =
	"missing", coef = "missing", trunc = "missing", oct = "ANY", S =
	"missing", N = "missing")}}{ number of species in each octave
      predicted from an abundance distribution model fitted with function
      \code{fitsad}.
    }
    \item{\code{signature(object = "missing", sad = "character", rad =
	"missing", coef = "list", trunc = "ANY", oct = "ANY", S = "numeric",
	N = "numeric")}}{number of species in each octave predicted from an
      abundance distribution named by \code{sad} with parameters
      defined in \code{coef}. 
    }
    \item{\code{signature(object = "numeric", sad = "character", rad =
	"missing", coef = "list", trunc = "ANY", oct = "ANY", S = "missing",
	N = "missing")}}{same as previous method, but with \code{S} and
      \code{N} taken from a vector of abundances given by \code{object}.
    }
    \item{\code{signature(object = "missing", sad = "missing", rad =
	"character", coef = "list", trunc = "ANY", oct = "ANY", S =
	"numeric", N = "numeric")}}{number of species in each octave predicted from an
      rank-abundance distribution named by \code{rad} with parameters
      defined in \code{coef}. 
    }
    \item{\code{signature(object = "numeric", sad = "missing", rad =
	"character", coef = "list", trunc = "ANY", oct = "ANY", S =
	"missing", N = "missing")}}{same as previous method, but with \code{S} and
      \code{N} taken from a vector of abundances given by \code{object}.
    }
  }
}

\arguments{
  \item{object}{an object of class \code{fitrad} or \code{fitrad};
    fitted model of rank-abundance or species abundances
    distributions. Alternatively a numeric vector abundances of species.
  }
  \item{sad, rad}{character; root name of sad or rad distribution to
    calculate expected percentiles. See \code{\link{fitsad}} and
    \code{\link{fitrad}} for available distributions.
  }
  \item{coef}{named list of numeric values; parameter values of the
    distribution given in \code{sad} or \code{rad}.
    Parameters should be named as in the corresponding density
    function, and in the same order.
  }
  \item{trunc}{non-negative integer, \code{trunc > min(x)}; truncation point
    if fitted distribution is truncated.
  }
  \item{oct}{integer vector; the octaves to tabulate abundances, see \link{octav}.
  }
  \item{S}{positive integer; number of species in the sample.
  }
  \item{N}{positive integer; number of individuals in the sample.
  }
}

\keyword{methods}

\seealso{\code{\link{octav}} and \code{\link{octav-class}} for generic
  function and methods to create an octave plot and details on abundance
  octaves; \code{\link{fitsad-class}} and 
  \code{\link{fitrad-class}} objects, from which you can also get an
  object of class \code{octav} with observed and predicted values;
  man page of \code{prestonfit} in package \pkg{vegan} for a
  detailed account of Preston's octaves and an alternative way to get
  octaves and fitting of species abundances distributions.  
}

\author{Paulo I Prado \email{prado@ib.usp.br} and Murilo Dantas Miranda.}

\examples{
## Predicted frequencies from a fitted model
## meta-community zero-sum multinomial for BCI data
bci.mzsm <- fitsad(bci, "mzsm")
bci.mzsm.oc <- octavpred(bci.mzsm)
## Preston plot with observed and predicted frequencies
plot(octav(bci))
lines(bci.mzsm.oc)
## Alternative model: local zero-sum multinomial
## Alonso & Mckane (Ecol. Lett. 2004, table 1) give theta = 44 and m = 0.15
bci.lzsm.oc <- octavpred( bci, sad = "volkov", coef =list(theta = 44, m = 0.15, J=sum(bci)) )
## Adding predicted frequencies to the plot
lines(bci.lzsm.oc, col = "red")
}
