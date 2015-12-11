\name{likelregions-class}

\docType{class}

\alias{likelregions-class}
\alias{show,likelregions-method}

\title{Class \code{"likelregions"} for likelihood profiles of maximum likelihood fits}

\description{
  This class provides a list of likelihood intervals for the parameters of
  a maximum likelihood fit. See the help on the function \code{\link{likelregions}} for details.
}

\examples{
x1.gamma <- fitgamma(moths)
x1.p <- profile(x1.gamma)
likelregions(x1.p)
# Compare with...
confint(x1.p)
}

\keyword{classes}
