\name{fitsadC-class}

\Rdversion{1.1}
\docType{class}
\alias{fitsadC-class}
%%\alias{coefficients,fitsadC-method}
%\alias{coverpred,fitsadC,missing,missing,missing,missing,missing,missing-method}
%%\alias{fitted,fitsadC-method}
%%\alias{fitted.values,fitsadC-method}
\alias{nobs,fitsadC-method}
\alias{plot,fitsadC,ANY-method}
\alias{ppsad,fitsadC,missing,missing,missing-method}
\alias{qqsad,fitsadC,missing,missing,missing,missing-method}
%% \alias{radpred,fitsadC,missing,missing,missing,missing,missing,missing,missing-method}
%%\alias{residuals,fitsadC-method}
\alias{show,fitsadC-method}

\title{Class \code{"fitsadC"} for maximum likelihood fitting of
  species abundance distributions from data in abundance classes}

\description{
  This class extends \code{mle2-class} to encapsulate models of species
  abundance distributions (SADs) fitted by maximum likelihood, from data
  where species are classified in abundance classes (e.g, histograms or
  frequency tables of number of species in classes of abundances).
}

\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("fitsadC",
    ...)}, or, more commonly a call to functions \code{\link{fitexpC}},
  \code{\link{fitgammaC}}, \code{\link{fitlnormC}},
  \code{\link{fitparetoC}}, \code{\link{fitweibullC}}, which fit a
  probability distribution to a table of frequency of species
  abundances.
}

\section{Slots}{
  \describe{
    \item{\code{sad}:}{Object of class \code{"character"}; root name of
      the species abundance distribution fitted. See man page of
      \code{fitsad} for available models.}
    \item{\code{trunc}:}{Object of class \code{"numeric"}; truncation
      value used in the fitted model. 'NA' for a non-truncated
      distribution.}
        \item{\code{hist}:}{Object of class \code{"histogram"}; a table of
  frequencies of species in abundance classes, returned by the function \code{hist}.}
    \item{\code{call}:}{Object of class \code{"language"}; The call to \code{\link{mle2}}.}
    \item{\code{call.orig}:}{Object of class \code{"language"} The call to \code{\link{mle2}},
      saved in its original form (i.e. without data arguments
      evaluated).}
    \item{\code{coef}:}{Object of class \code{"numeric"}; Vector of estimated parameters.}
    \item{\code{fullcoef}:}{Object of class \code{"numeric"}; Fixed and estimated parameters. }
    \item{\code{vcov}:}{Object of class \code{"matrix"}; Approximate variance-covariance
      matrix, based on the second derivative matrix at the MLE.}
    \item{\code{min}:}{Object of class \code{"numeric"}; Minimum value of objective function =
      minimum negative log-likelihood.}
    \item{\code{details}:}{Object of class \code{"list"}; Return value from \code{\link{optim}}.}
    \item{\code{minuslogl}:}{Object of class \code{"function"}; The negative log-likelihood
      function.}
    \item{\code{method}:}{Object of class \code{"character"}; The optimization method used. }
    \item{\code{data}:}{Object of class \code{"data.frame"}; Data with which to evaluate the negative log-likelihood function.}
    \item{\code{formula}:}{Object of class \code{"character"}; If a formula was specified, a
      character vector giving the formula and parameter specifications. }
    \item{\code{optimizer}:}{Object of class \code{"character"}; The
      optimizing function used. }
  }
}

\section{Extends}{
  Class \code{"\linkS4class{mle2}"}, directly.
}


\section{Methods}{
  \describe{
    %%\item{coefficients}{\code{signature(object = "fitsadC")}: ... }
    \item{coverpred}{\code{signature(object = "fitsadC", sad =
	"missing", coef = "missing", trunc = "missing",
	breaks = "missing", mids = "missing", S = "missing")}:
      predicted number of species in
      each abundance class see \code{\link{coverpred}} }
    %%\item{fitted}{\code{signature(object = "fitsadC")}: ... }
    %%\item{fitted.values}{\code{signature(object = "fitsadC")}: ... }
    \item{nobs}{\code{signature(object = "fitsadC")}: Displays number of
      observations (number of species) in the data to which the model was fitted.}
    \item{plot}{\code{signature(x = "fitsadC", y = "ANY")}: diagnostic
      plots of the fitted model. }
    \item{ppsad}{\code{signature(x = "fitsadC", sad = "missing", coef =
	"missing", trunc = "missing")}:
      plot of observed vs predicted percentiles of the abundance
      distribution, details in \code{\link{ppsad}}.  }
    \item{qqsad}{\code{signature(x = "fitsadC", sad = "missing", coef =
	"missing", trunc = "missing", distr = "missing")}: plot of observed vs
      predicted quantiles of the abundance distribution, details in
      \code{\link{qqsad}. } }
    \item{radpred}{\code{signature(object = "fitsadC", sad = "missing",
	rad = "missing", coef = "missing", trunc = "missing",
	distr = "missing", S = "missing", N = "missing")}:
      expected abundances of the 1st to n-th most abundant species, see \code{\link{rad}}
      and \code{\link{radpred}}.  }
    %%\item{residuals}{\code{signature(object = "fitsadC")}: ... }
    \item{show}{\code{signature(object = "fitsadC")}: Displays object.}
  } 
}




\references{
  Bolker, B. and R Development Core Team 2012. bbmle: Tools for
  general maximum likelihood estimation. R package version 1.0.5.2.
  http://CRAN.R-project.org/package=bbmle
}

\author{Paulo I Prado \email{prado@ib.usp.br},
  after Ben Bolker and  R Core Team.}


\note{Class \code{fitsadC} only adds three slots to class
  \code{mle2}. The descriptions of slots inherited from \code{mle2-class}
  replicate those in \code{\link{mle2-class}}. 
}

\source{this class builds on \code{mle2-class} of \pkg{bbmle} package (Bolker
  2012), which in turn builds on \code{mle-class}. 
}

\seealso{\code{\link{mle2-class}} for all methods available from which
  \code{fitsadC-class} inherits; \code{\link{fitsadC}} for details on
  fitting SADs models from frequency tables; \code{\link{coverpred}} to
  get  frequencies of species in abundances classes predicted 
  from fitted models.
}

\examples{
## Example of fitting a sad model to cover data 
## Abundance classes: cover scale for plants
Lbrk <- c(0,1,3,5,15,25,35,45,55,65,75,85,95,100)
## To fit a sad model to cover data, data sould be in histogram format
grass.h <- hist(grasslands$mids, breaks = Lbrk, plot = FALSE)
class(grass.h) ## class "histogram"
## Fits a Pareto distribution to the histogram object
grass.p <- fitparetoC(grass.h)
class(grass.p)
## The class has a plot method to show diagnostic plots
par(mfrow=c(2,2))
plot(grass.p)
par(mfrow=c(1,1))
## Some methods inherited form mle2-class
summary(grass.p)
coef(grass.p)
AIC(grass.p)
}

\keyword{classes}
