\name{ICtab}
\docType{methods}
\alias{nobs}
\alias{AIC}
\alias{AICc}
\alias{AIC-methods}
\alias{AICc-methods}
\alias{nobs-methods}
\alias{AIC,mle2-method}
\alias{AICc,mle2-method}
\alias{nobs,fitrad-method}
\alias{nobs,fitsad-method}

\title{Compute table of information criteria and auxiliary info}

\description{
	This functions reimplement some functionality from package bbmle
	to work better with objects from the classes \code{\link{fitsad-class}}
	and \code{\link{fitrad-class}}. 
}

\section{Methods}{
  \describe{
    \item{AIC}{\code{signature("mle2")}: Akaike information criterion. }
    \item{AICc}{\code{signature("mle2")}: Akaike information
      criterion corrected for small samples. }
    \item{nobs}{\code{signature(object = "fitsad")}: Number of observations. }
    \item{nobs}{\code{signature(object = "fitrad")}: Number of observations. }
  }
}

\author{Paulo I Prado \email{prado@ib.usp.br}, Andre Chalom and Murilo Dantas Miranda,
  after Ben Bolker and  R Core Team.}


\seealso{\code{\link{mle2-class}} for all methods available from which
  \code{fitsad-class} inherits; \code{\link{fitsad}} for details on
  fitting SADs models.
}
