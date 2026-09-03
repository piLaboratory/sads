\name{updatesad}
\alias{updatesad}
\alias{updaterad}

\title{Updating of MLE fits by profiling}

\description{These functions update a fitsad/fitrad object
  by running the optimizer again starting on better fit
  returned by profile. 
  
  These functions were not extensively tested, and should
  be considered in beta testing phase.
}

\usage{
updatesad(object, \dots) 
updaterad(object, \dots) 
}

\arguments{
  \item{object}{
    object of the class \code{\link{fitsad-class}} or \code{\link{fitrad-class}}
    for which the optimization did not converge, or converged to a local minimum.
  }
  
  \item{\dots}{
    list of additional parameters to be passed to the \code{fitsad} or \code{fitrad} fitting
    procedure. May include a different optimizer, method, or upper/lower bounds for the parameters.
    Refer to the \code{fitsad} and \code{fitrad} man pages for details of which parameters may be
    specified.
  }
}

\value{
  An object of \code{\link{fitsad-class}} or \code{\link{fitrad-class}}.
}

\details{
  The \code{updatesad} function runs a new profile of the fitted object, and if the
  profile is able to find a better fit, it runs a new optimization starting on this better fit.
  If the profiling does not find a new fit, the function exits with error.
  The actual processing is done by \code{updatesad}. \code{updaterad} is simply a convenience alias.

  Truncation settings (\code{trunc} and \code{trunc.max}) of the original
  fitted object are preserved in the object returned by \code{updatesad} and
  \code{updaterad}.
}

\seealso{
  \code{\link{fitsad-class}}, \code{\link{fitrad-class}}, \code{\link{fitsadC-class}}
  }



\author{Paulo I Prado \email{prado@ib.usp.br} and Andre Chalom}

\examples{
## An artificially poorly-converged fit (few iterations allowed),
## truncated at both a minimum and a maximum abundance
data(moths)
moths.ln <- fitsad(moths, "lnorm", trunc = 0.5, trunc.max = 2500,
                    start.value = c(meanlog = 1, sdlog = 1),
                    control = list(maxit = 2))
## updatesad finds a better fit and preserves the truncation settings
moths.ln2 <- updatesad(moths.ln)
moths.ln2@trunc.max
}
