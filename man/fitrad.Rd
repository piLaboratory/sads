\name{fitrad}

\alias{fitrad}
\alias{fitgs}
\alias{fitmand}
\alias{fitrbs}
\alias{fitzipf}

\title{ML fitting of species rank-abundance distributions}

\description{Fits probability distributions for abundance ranks
  of species in a sample or assemblage by maximum likelihood.
}

\usage{
fitrad(x, rad =c("gs", "mand", "rbs", "zipf"), \dots)
fitgs(x, trunc, \dots)
fitmand(x, trunc, start.value, \dots)
fitrbs(x, trunc, \dots)
fitzipf(x, N, trunc, start.value, upper = 20, \dots)
}

\arguments{
  \item{x}{vector of (positive integer) quantiles or an object of
    \code{\link{rad-class}}. 
    In the context of
    rads, the numerical vector contains abundances of species in a sample or ecological
    assemblage according to their abundance. The \code{\link{rad-class}} object
    contains ranked abundances of species in a sample or ecological
    assemblage.
  }
  \item{rad}{
    character; root name of community rad distribution to be fitted.
    \kbd{"gs"} for geometric series (not geometric distribution,
    \code{\link{dgeom}}),
    \kbd{"mand"} for Zipf-Mandelbrodt distribution,
    \kbd{"rbs"} for MacArthur's Broken-stick distribution,
    \kbd{"zipf"} for Zipf distribution.
  }

  \item{trunc}{
    non-negative integer, \code{trunc > min(x)}; truncation point to fit a
    truncated distribution.
  }

  \item{N}{
    positive integer, total number of individuals in the sample/assemblage.
  }
  
  \item{start.value}{
    numeric named vector; starting values of free parameters to be
    passed to \code{mle2}.
    Parameters should be named as in the corresponding density
    function, and in the same order.
  }
  
  %% \item{trueLL}{
  %%   logical; use 'true likelihood' for continuous models? (see \link{trueLL}).
  %% }

  %% \item{dec.places}{
  %%   positive integer; number of decimal places used in the measurement of
  %%   the observed values. Observed values will be rounded to this number of
  %%   decimals. This argument defines the measurement precision for
  %%   abundance variables. Used for continuous distributions when
  %%   argument \code{trueLL=TRUE} 
  %%   (see details and \link{trueLL}).
  %% }

  \item{upper}{
    real positive; upper bound for the Brent's one-parameter optimization
    method (default), for fits that use this method by default. See
    details and \code{optim}.
  }
  
  \item{\dots}{
    in fitrad further arguments to be passed to the specific fitting
    function (most used are \code{trunc} and
    \code{start.value}%%and\code{trueLL}
    ). In the specific fitting functions further
    arguments to be passed to \code{mle2}.
  }
}

\value{
  An object of \code{\link{fitrad-class}} which inherits from \code{mle2-class}
  and thus has methods for handling
  results of maximum likelihood fits from \code{mle2}
  and also specific methods to handle rank-abundance models.
}

\details{
  All these functions fit rank-abundance distributions (RAD) to a vector of
  abundances or a rank-abundance table of the \code{\link{rad-class}}.
  RADs assign probabilities p(i) to each rank i, which can be interpreted as
  the expected proportion of total individuals in the sample that are of
  the i-th species.
  
  \code{fitrad} is simply a wrapper that calls the specific functions to fit
  the distribution chosen with the argument \code{rad}. Users
  can interchangeably use \code{fitrad} or the individual functions
  detailed below 
  (e.g. \code{fitrad(x, sad="rbs", \dots)} is the same as
  \code{fitrbs(x, \dots)} and so on).
  
  The distributions are fitted by the
  maximum likelihood method using numerical optimization,
  with \code{mle2}.
  The resulting object is of \code{fitrad-class}
  which can be handled with \code{mle2} methods
  for fitted models and has also some additional
  methods for RADs models (see
  \code{\link{fitrad-class}} and examples).
  By default, fitting to one-parameter distributions (\code{fitgs},
  \code{fitzipf}) uses Brent's one-dimensional method of optimization (see
  \code{\link{optim}}).

  \code{fitgs} fits Motomura's  Geometric Series (Whittaker
  1965, May 1975) to abundance ranks. 
  This was the first model fitted to species
  abundance data (Motomura 1932, \emph{apud} Doi and Mori 2012),
  which was subsequently described as the result
  of niche pre-emption at a constant rate (Numata et. al. 1953 \emph{apud} Doi
  and Mori 2012). 
  
  
  \code{fitrbs} fits the Broken-stick distribution
  (MacArthur 1960) to abundance ranks. It is defined only by the observed number of
  elements \code{S} in the collection and collection size \code{N}.
  Therefore, once a sample is taken,
  the Broken-stick has no free parameters.
  Therefore, there is no actual fitting, but still
  the \code{fitrbs} calls
  \code{mle2} with
  fixed parameters N and S and \code{eval.only=TRUE}
  to return an object of \code{fitrad-class}
  to keep compatibility with other
  RAD models fitted to the same data.
  Therefore the resulting objects allows most of the
  operations with RAD models, such as
  comparison with other models through model selection,
  diagnostic plots and so on
  (see \link{fitrad-class}).

  \code{fitzipf} and \code{fitmand} fit the Zipf distribution and its
  two-parameter generalization, the Zipf-Mandelbrodt distribution. Both
  are discrete power-law distributions commonly proposed as RAD models,
  though they in general provide poor fit to species abundances (Newman 2005).
  
}

\references{

  Bolker, B. and R Development Core Team 2012. bbmle: Tools for
  general maximum likelihood estimation. R package version 1.0.5.2.
  http://CRAN.R-project.org/package=bbmle

  Doi, H. and Mori, T. 2012. The discovery of species-abundance
  distribution in an ecological community. \emph{Oikos 122:} 179--182.

  MacArthur, R.H. 1960. On the relative abundance of species.
  \emph{Am Nat 94}:25--36.

  May, R.M. 1975. Patterns of Species Abundance and Diversity. In
  Cody, M.L. and Diamond, J.M. (Eds) \emph{Ecology and Evolution of
    Communities}. Harvard University Press. pp 81--120.

  Newman, M.E.J. 2005. Power laws, Pareto distributions and Zipf's law.
  \emph{Contemporary Physics, 46:} 323--351.
  
  Whittaker, R.H. 1965. Dominance and diversity in land plant
  communities. \emph{Science 147}: 250--260.
  
}

\source{all fitting functions builds on \code{mle2} and methods
  from 'bbmle' package (Bolker 2012), which in turn builds on
  \code{mle} function and associated classes and methods.
}

\seealso{
  \code{\link{dgs}}, \code{\link{dmand}}, \code{\link{drbs}},
  \code{\link{dzipf}},
  for corresponding density functions created for fitting RADs;
  \code{\link{fitrad-class}}.
}

\examples{

## Figure 2 of Motomura (1932)
data(okland)
plot(rad(okland))
ok.gs <- fitrad(okland, "gs")
lines(radpred(ok.gs))

## Comparison with Zipf-Mandelbrodt
ok.zm <- fitrad(okland, "mand")
AICctab(ok.gs, ok.zm, nobs=length(okland))
lines(radpred(ok.zm), col="red")
}

\author{Paulo I Prado \email{prado@ib.usp.br} and Murilo Dantas Miranda}

