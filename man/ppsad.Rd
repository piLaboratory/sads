\name{ppsad-methods}


\docType{methods}

\alias{ppsad-methods}
\alias{ppsad,fitsad,missing,missing,missing,missing-method}
\alias{ppsad,numeric,character,list,ANY,character-method}
\alias{pprad-methods}
\alias{pprad,fitrad,missing,missing,missing,missing-method}
\alias{pprad,rad,character,list,ANY,character-method}
\alias{pprad,numeric,character,list,ANY,character-method}

\title{Percentile-percentile plots for species-abundance and
  rank-abundance models}

\description{
Plots empirical percentiles vs corresponding theoretical values
expected by a model for species abundances (SAD) or a model for species abundance
ranks (RAD).
}

\section{Methods}{

  \describe{

    \item{ppsad}{\code{signature(x = "fitsad", sad = "missing", coef =
      "missing", trunc = "missing", distr = "missing", plot="ANY", line="ANY")}:
      quantile-quantile plot for a fitted model of species abundances (a
      \link{\code{fitsad-class}} object). Only argument \code{x} should be provided
    }
    \item{ppsad}{\code{signature(x = "numeric", sad = "character", coef =
      "list", trunc = "ANY", distr = "character", plot="ANY", line="ANY")}:
      quantile-quantile plot of a numeric vector of abundances (\code{x}) vs a species
      abundance distributions defined by the following arguments.
    }
    \item{pprad}{\code{signature(x = "fitrad", rad = "missing", coef =
      "missing", trunc = "missing", plot="ANY", line="ANY")}:
      quantile-quantile plot for a fitted model of species abundances
      ranks (a \link{\code{fitrad-class}} object). Only argument \code{x} should be provided
    }
    \item{pprad}{\code{signature(x = "rad", rad = "character", coef =
      "list", trunc = "ANY", plot="ANY", line="ANY")}:
      quantile-quantile plot of a table of abundance ranks (\code{x}) vs a species
      rank-abundance distribution defined by the following arguments.
    }    
    \item{pprad}{\code{signature(x = "numeric", rad = "character", coef =
      "list", trunc = "ANY", plot="ANY", line="ANY")}:
      quantile-quantile plot of a numeric vector of abundances (\code{x}) vs a species
      rank-abundance distribution defined by the following arguments.
    }
  }
}

\usage{
\S4method{ppsad}{fitsad}(x, plot=TRUE, line=TRUE, ...)
\S4method{ppsad}{numeric}(x, sad, coef, trunc=NA, distr, plot=TRUE,
line=TRUE, ...)
\S4method{pprad}{fitrad}(x, plot=TRUE, line=TRUE, ...)
\S4method{pprad}{rad}(x, rad, coef, trunc=NA, plot=TRUE, line=TRUE, ...)
\S4method{pprad}{numeric}(x, rad, coef, trunc=NA, plot=TRUE, line=TRUE, ...)
}

\arguments{
  \item{x}{ 
    a numeric vector of abundances of
  species or a fitted sad/rad model (object of \code{\link{fitsad-class}}
  or \code{\link{fitrad-class}}, respectively).
  }
  \item{sad, rad}{character; root name of sad or rad
  distribution to calculate expected percentiles. See \link{\code{fitsad}} and
  \link{\code{fitrad}} for available distributions.
  }
  \item{coef}{named list of numeric values; parameter values of the
    distribution given in \code{sad} or \code{rad}.
    Parameters should be named as in the corresponding density
    function, and in the same order
  }
  \item{trunc}{non-negative integer, trunc > min(x); truncation point to fit a
    truncated distribution.
  }
  \item{distr}{
    character; type of distribution given in \code{sad}: "D" for
  discrete or "C" for continuous.
  }
  \item{plot}{
    logical; if 'TRUE' a percentile-percentile plot is produced. If not,
  a dataframe with theoretical and empirical values for percentiles of
  the data is invisibly returned.
  }
  \item{line}{
    logical; if 'TRUE' and \code{plot} is 'TRUE', the equivalence line y=x with
    \code{abline(0,1)} is added to the plot. If not, no line is drawn.
  }
  \item{...}{ further arguments to be passed to the \code{plot} function.
    }
}
\keyword{methods}

\examples{
## An example with SADs
data(moths)
## fits log-series distribution to abundance data
moths.ls <- fitsad(moths, "ls")
## fits lognormal distribution truncated at 0.5
moths.ln <- fitsad(moths,"lnorm", trunc=0.5)
## Plots with the model object and with abundance vector 
par(mfrow=c(2,2))
ppsad(moths.ls)
ppsad(moths, sad="ls", coef=as.list(coef(moths.ls)))
ppsad(moths.ln)
ppsad(moths, sad="lnorm", coef=as.list(coef(moths.ln)), trunc=0.5)
par(mfrow=c(1,1))

## An example with RADs
data(okland)
## Fits broken-stick RAD model
ok.bs <- fitrad(okland, "rbs")
## Fits geometric series RAD model
ok.gs <- fitrad(okland, "gs")
## Plots with the model object and with the abundance vector 
par( mfrow=c(2, 2) )
pprad(ok.bs)
pprad(okland, rad="rbs", coef=as.list(coef(ok.bs)))
pprad(ok.gs)
pprad(okland, rad="gs", coef=as.list(coef(ok.gs)))
par(mfrow=c(1,1))
}
