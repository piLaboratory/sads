\name{fitrad-class}

\docType{class}

\alias{fitrad-class}
%\alias{octavpred,fitrad,missing,missing,missing,missing,ANY,missing,missing-method}
\alias{plot,fitrad,ANY-method}
\alias{show,fitrad-method}
%\alias{pprad,fitrad-method}
\alias{pprad,fitrad,missing,missing-method}
\alias{pprad,fitrad,missing,missing,missing,missing-method}
%% \alias{qqrad,fitrad-method}
\alias{qqrad,fitrad,missing,missing,missing,missing-method}
%% \alias{octavpred,fitrad,missing,missing,missing,missing,ANY,missing,missing-method}
%% \alias{radpred,fitrad,missing,missing,missing,missing,missing,missing,missing-method}

\title{Class \code{"fitrad"} for maximum likelihood fitting of species
  rank-abundance distributions}

\description{
  This class extends \code{mle2-class} to encapsulate models of species
  rank-abundance distributions 
  (RADs) fitted by maximum likelihood.
}

\section{Objects from the Class}{
  Objects created by a call to function \code{\link{fitrad}}, which fits
  a probability distribution to an abundance vector.
}

\section{Slots}{
  \describe{
    \item{\code{rad}:}{Object of class \code{"character"}; root name of
      the species abundance distribution fitted. See man page of
      \code{fitrad} for available models.}
    \item{\code{distr}:}{Deprecated since sads 0.2.4. See \code{\link{distr}} function}
    \item{\code{trunc}:}{Object of class \code{"numeric"}; truncation
      value used in the fitted model. 'NA' for a non-truncated
      distribution.}
    \item{\code{rad.tab}:}{Object of class \code{"rad"}; rank-abundance
      table of observed abundances.}
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
    \item{octavpred}{\code{signature(object = "fitrad", sad = "missing",
	rad = "missing", coef = "missing", trunc = "missing", oct = "ANY", S =
	"missing", N = "missing")}: expected number of species per
      abundance octave, see \code{\link{octav}} and \code{\link{octavpred}}.} 
    \item{plot}{\code{signature(x = "fitrad", y = "ANY")}: diagnostic
      plots of the fitted model. }
    \item{show}{\code{signature(object = "fitrad")}: Displays object.}
    \item{nobs}{\code{signature(object = "fitrad")}: Displays number of
      observations (number of species) in the data to which the model was fitted.}
    \item{pprad}{\code{signature(x = "fitrad", sad = "missing", coef =
	"missing", trunc = "missing")}: plot of observed vs predicted
      percentiles of the abundance distribution, details in
      \code{\link{pprad}}. } 
    \item{qqrad}{\code{signature(x = "fitrad", sad = "missing", coef =
	"missing", trunc = "missing")}: plot of observed vs predicted
      quantiles of the abundance distribution, details in
      \code{\link{qqrad}. } }
    \item{radpred}{\code{signature(object = "fitrad", sad = "missing",
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


\note{Class \code{fitrad} only adds four slots to class
  \code{mle2}. The descriptions of slots inherited from \code{mle2-class}
  replicate those in \code{\link{mle2-class}}. 
}

\source{this class builds on \code{mle2-class} of \pkg{bbmle} package (Bolker
  2012), which in turn builds on \code{mle-class}. 
}

\seealso{\code{\link{mle2-class}} for all methods available from which
  \code{fitrad-class} inherits; \code{\link{fitrad}} for details on
  fitting RADs models; \code{\link{octavpred}} and
  \code{\link{radpred}} to get rank-abundance and
  frequencies of species in octaves predicted 
  from fitted models.
}

\examples{
ok.gser <- fitrad(okland, "gs")
## The class has a plot method to show diagnostic plots
par(mfrow=c(2,2))
plot(ok.gser)
# The same plot, but with relative abundances
plot(ok.gser, prop = TRUE)
par(mfrow=c(1,1))
## Some useful methods inherited from mle2-class
coef(ok.gser)
confint(ok.gser)
logLik(ok.gser)
## Model selection
ok.zipf <- fitrad(okland, "zipf")
AICctab(ok.gser, ok.zipf, nobs=length(moths), base=TRUE)
}

\keyword{classes}
