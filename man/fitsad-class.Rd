\name{fitsad-class}

\docType{class}

\alias{fitsad-class}
\alias{plot,fitsad,ANY-method}
\alias{show,fitsad-method}
%\alias{ppsad,fitsad-method}
\alias{ppsad,fitsad,missing,missing-method}
\alias{ppsad,fitsad,missing,missing,missing-method}
%\alias{qqsad,fitsad-method}
\alias{qqsad,fitsad,missing,missing,missing,missing-method}
%\alias{octavpred,fitsad,missing,missing,missing,missing,ANY,missing,missing-method}
%\alias{radpred,fitsad,missing,missing,missing,missing,missing,missing,missing-method}

\title{Class \code{"fitsad"} for maximum likelihood fitting of
  species abundance distributions}

\description{
  This class extends \code{mle2-class} to encapsulate models of species
  abundance distributions (SADs) fitted by maximum likelihood.
}

\section{Objects from the Class}{
  Objects created by a call to function \code{\link{fitsad}}, which fits a
  probability distribution to an abundance vector.
}

\section{Slots}{
  \describe{
    \item{\code{sad}:}{Object of class \code{"character"}; root name of
      the species abundance distribution fitted. See man page of
      \code{fitsad} for available models.}
    \item{\code{distr}:}{Deprecated since sads 0.2.4. See \code{\link{distr}} function}
    \item{\code{trunc}:}{Object of class \code{"numeric"}; truncation
      value used in the fitted model. 'NA' for a non-truncated distribution.}
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
    \item{\code{optimizer}:}{Object of class \code{"character"}; The optimizing function used. }
  }
}


\section{Extends}{
  Class \code{"\linkS4class{mle2}"}, directly.
}

\section{Methods}{
  \describe{
    \item{octavpred}{\code{signature(object = "fitsad", sad = "missing",
	rad = "missing", coef = "missing", trunc = "missing", oct = "ANY", S =
	"missing", N = "missing")}: expected number of species per
      abundance octave, see \code{\link{octav}} and \code{\link{octavpred}}.} 
    \item{plot}{\code{signature(x = "fitsad", y = "ANY")}: diagnostic
      plots of the fitted model. }
    \item{show}{\code{signature(object = "fitsad")}: Displays object.}
    \item{ppsad}{\code{signature(x = "fitsad", sad = "missing", coef =
	"missing", trunc = "missing")}: plot of observed vs predicted
      percentiles of the abundance distribution, details in
      \code{\link{ppsad}}. } 
    \item{qqsad}{\code{signature(x = "fitsad", sad = "missing", coef =
	"missing", trunc = "missing", distr = "missing")}: plot of observed vs predicted
      quantiles of the abundance distribution, details in
      \code{\link{qqsad}. } }
    \item{radpred}{\code{signature(object = "fitsad", sad = "missing",
	rad = "missing", coef = "missing", trunc = "missing", distr =
	"missing", S = "missing", N = "missing")}: expected abundances
      of the 1st to n-th most abundant species, see \code{\link{rad}} and \code{\link{radpred}}. }
  }
}

\references{
  Bolker, B. and R Development Core Team 2012. bbmle: Tools for
  general maximum likelihood estimation. R package version 1.0.5.2.
  http://CRAN.R-project.org/package=bbmle
}

\author{Paulo I Prado \email{prado@ib.usp.br} and Murilo Dantas Miranda,
  after Ben Bolker and  R Core Team.}


\note{Class \code{fitsad} only adds three slots to class
  \code{mle2}. The descriptions of slots inherited from \code{mle2-class}
  replicate those in \code{\link{mle2-class}}. 
}

\source{this class builds on \code{mle2-class} of \pkg{bbmle} package (Bolker
  2012), which in turn builds on \code{mle-class}. 
}

\seealso{\code{\link{mle2-class}} for all methods available from which
  \code{fitsad-class} inherits; \code{\link{fitsad}} for details on
  fitting SADs models; \code{\link{octavpred}} and
  \code{\link{radpred}} to get rank-abundance and
  frequencies of species in octaves predicted 
  from fitted models.
}

\examples{
moths.ls <- fitsad(moths, "ls")
## The class has a plot method to show diagnostic plots
par(mfrow=c(2,2))
plot(moths.ls)
# the same plot, but with relative abundances
plot(moths.ls, prop = TRUE) 
par(mfrow=c(1,1))
## Some useful methods inherited from mle2-class
coef(moths.ls)
confint(moths.ls)
logLik(moths.ls)
## Model selection
moths.ln <- fitsad(moths, "lnorm", trunc=0.5)
AICctab(moths.ls, moths.ln, nobs=length(moths), base=TRUE)
}

\keyword{classes}
