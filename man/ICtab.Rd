\name{ICtab}
\docType{methods}
\alias{nobs}
\alias{nobs-methods}
\alias{nobs,fitrad-method}
\alias{nobs,fitsad-method}

\title{Compute number of observations}

\description{
	This functions is needed for calculating AICc. These may be removed in a future release.
}

\section{Methods}{
  \describe{
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
