\name{qqsad and qqrad methods}

\docType{methods}

\alias{qqsad}
\alias{qqrad}
%% \alias{qqsad-methods}
\alias{qqsad,fitsad-method}
%% \alias{qqsad,fitsad,missing,missing,missing,missing-method}
\alias{qqsad,integer-method}
\alias{qqsad,numeric-method}
\alias{qqsad,numeric,character,list,ANY,character-method}
%% \alias{qqrad-methods}
\alias{qqrad,fitrad-method}
\alias{qqrad,fitrad,missing,missing,missing-method}
%\alias{qqrad,fitrad,missing,missing,missing,missing-method}
\alias{qqrad,integer-method}
\alias{qqrad,numeric-method}
\alias{qqrad,numeric,character,list,ANY-method}
\alias{qqrad,numeric,character,list,ANY,character-method}
\alias{qqrad,rad-method}
\alias{qqrad,rad,character,list,ANY-method}
\alias{qqrad,rad,character,list,ANY,character-method}

\title{Quantile-quantile plots for species-abundance and
  rank-abundance models}

\description{
  Plots empirical quantiles vs corresponding theoretical values
  expected by a model for species abundances (SAD) or a model for species abundance
  ranks (RAD).
}

\section{Methods}{

  \describe{

    \item{qqsad}{\code{signature(x = "fitsad", sad = "missing", coef =
	"missing", trunc = "missing", distr = "missing", plot="ANY", line="ANY")}:
      quantile-quantile plot for a fitted model of species abundances (a
      \code{\link{fitsad-class}} object). Only argument \code{x} should be provided
    }
    \item{qqsad}{\code{signature(x = "numeric", sad = "character", coef =
	"list", trunc = "ANY", distr = "character", plot="ANY", line="ANY")}:
      quantile-quantile plot of a numeric vector of abundances (\code{x}) vs a species
      abundance distributions defined by the following arguments.
    }
    \item{qqrad}{\code{signature(x = "fitrad", rad = "missing", coef =
	"missing", trunc = "missing", plot="ANY", line="ANY")}:
      quantile-quantile plot for a fitted model of species abundances
      ranks (a \code{\link{fitrad-class}} object). Only argument \code{x} should be provided
    }
    \item{qqrad}{\code{signature(x = "rad", rad = "character", coef =
	"list", trunc = "ANY", plot="ANY", line="ANY")}:
      quantile-quantile plot of a table of abundance ranks (\code{x}) vs a species
      rank-abundance distribution defined by the following arguments.
    }    
    \item{qqrad}{\code{signature(x = "numeric", rad = "character", coef =
	"list", trunc = "ANY", plot="ANY", line="ANY")}:
      quantile-quantile plot of a numeric vector of abundances (\code{x}) vs a species
      rank-abundance distribution defined by the following arguments.
    }
  }
}

\usage{
\S4method{qqsad}{fitsad}(x, plot=TRUE, line=TRUE, \dots)
\S4method{qqsad}{numeric}(x, sad, coef, trunc=NA, distr, plot=TRUE,
line=TRUE, \dots)
\S4method{qqrad}{fitrad}(x, plot=TRUE, line=TRUE, \dots)
\S4method{qqrad}{rad}(x, rad, coef, trunc=NA, plot=TRUE, line=TRUE, \dots)
\S4method{qqrad}{numeric}(x, rad, coef, trunc=NA, plot=TRUE, line=TRUE, \dots)
}

\arguments{
  \item{x}{ 
    a numeric vector of abundances of
    species or a fitted sad/rad model (object of \code{\link{fitsad-class}}
    or \code{\link{fitrad-class}}, respectively). For \code{qqrad} this
    argument can be also a rank-abundance table of \code{\link{rad-class}}.
  }
  \item{sad, rad}{character; root name of sad or rad
    distribution to calculate expected percentiles. See \code{\link{fitsad}} and
    \code{\link{fitrad}} for available distributions.
  }
  \item{coef}{named list of numeric values; parameter values of the
    distribution given in \code{sad} or \code{rad}.
    Parameters should be named as in the corresponding density
    function, and in the same order
  }
  \item{trunc}{non-negative integer, \code{trunc > min(x)}; truncation point to fit a
    truncated distribution.
  }
  \item{distr}{
    character; type of distribution given in \code{sad}: "D" for
    discrete or "C" for continuous.
  }
  \item{plot}{
    logical; if \kbd{TRUE} a percentile-percentile plot is produced. If not,
    only a data frame with theoretical and empirical values for percentiles of
    the data is invisibly returned.
  }
  \item{line}{
    logical; if \kbd{TRUE} and \code{plot} is \kbd{TRUE}, the equivalence line y=x with
    \code{abline(0,1)} is added to the plot. If not, no line is drawn.
  }
  \item{\dots}{ further arguments to be passed to the \code{plot} function.
  }
}

\keyword{methods}

\references{
  Thas, O. 2010. Comparing distributions. Springer.
}

\author{Paulo I Prado \email{prado@ib.usp.br} and Murilo Dantas Miranda.}

\examples{
## An example with SADs
data(moths)
## fits log-series distribution to abundance data
moths.ls <- fitsad(moths, "ls")
## fits lognormal distribution truncated at 0.5
moths.ln <- fitsad(moths,"lnorm", trunc=0.5)
## Plots with the model object and with abundance vector 
par(mfrow=c(2,2))
qqsad(moths.ls)
qqsad(moths, sad="ls", coef=as.list(coef(moths.ls)), distr="D")
qqsad(moths.ln)
qqsad(moths, sad="lnorm", coef=as.list(coef(moths.ln)), trunc=0.5, distr="C")
par(mfrow=c(1,1))

## An example with RADs
data(okland)
## Fits broken-stick RAD model
ok.bs <- fitrad(okland, "rbs")
## Fits geometric series RAD model
ok.gs <- fitrad(okland, "gs")
## Plots with the model object and with the abundance vector 
par( mfrow=c(2, 2) )
qqrad(ok.bs)
qqrad(okland, rad="rbs", coef=as.list(coef(ok.bs)))
qqrad(ok.gs)
qqrad(okland, rad="gs", coef=as.list(coef(ok.gs)))
par(mfrow=c(1,1))
}
