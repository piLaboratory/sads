\name{fitsadC}
\alias{fitsadC}
\alias{fitexpC}
\alias{fitgammaC}
\alias{fitlnormC}
\alias{fitparetoC}
\alias{fitweibullC}

\title{ML fitting of species abundance distributions to data pooled in abundance classes}

\description{Fits probability distributions for abundances of species
  aggregated in abundance classes in a sample or assemblage by maximum
  likelihood.}

\usage{
fitsadC(x, sad = c("exp","gamma","lnorm","pareto", "weibull"), \dots)

fitexpC(x, trunc = NULL, start.value, \dots)

fitgammaC(x, trunc, start.value,  \dots)

fitlnormC(x, trunc, start.value,  \dots)

fitparetoC(x, trunc, start.value,  upper = 20, \dots)

fitweibullC(x, trunc, start.value,  \dots)
}

\arguments{
  \item{x}{object of class \code{histogram} with the number of species in
    abundance classes, in a sample or ecological assemblage.
  }
  \item{sad}{
    character; root name of community sad distribution to be fitted.
    \kbd{"exp"} for exponential distribution,
    \kbd{"gamma"} for gamma distribution,
    \kbd{"lnorm"} for lognormal,
    \kbd{"pareto"} for Pareto distribution,
    \kbd{"weibull"} for Weibull distribution.
  }
  
  \item{trunc}{
    non-negative integer, \code{trunc > min(x)}; truncation point to fit a
    truncated distribution. 
  }
  
  \item{start.value}{
    named numeric vector; starting values of free parameters to be
    passed to \code{mle2}.
    Parameters should be named as in the corresponding density
    function,
    and in the same order.
  }
  
  \item{upper}{
    real positive; upper bound for Brent's one-parameter optimization
    method (default), for fits that use this method by default. See
    details and \code{optim}.
  }
  
  \item{\dots}{
    in \code{fitsad} further arguments to be passed to the specific fitting
    function (most used are \code{trunc}, \code{start.value})
    In the specific fitting functions further
    arguments to be passed to \code{mle2}.
  }
}

\value{
  An object of \code{fitsadC-class} which inherits from \code{mle2-class}
  and thus has methods for handling
  results of maximum likelihood fits from \code{mle2}
  and also specific methods to handle SADs models
  (see \code{\link{fitsadC-class}}).
}

\details{ \code{fitsadC} is simply a wrapper that calls the specific
  functions to fit the distribution chosen with the argument
  \code{sad}. Users can interchangeably use \code{fitsadC} or the
  individual functions detailed below (e.g. \code{fitsad(x, sad="exp",
    \dots)} is the same as \code{fitexpC(x, \dots)} and so on).
  
  The distributions are fitted by the maximum likelihood method using
  numerical optimization, with \code{mle2}.  The resulting object is of
  \code{fitsadC-class} which can be handled with \code{mle2} methods for
  fitted models and has also some additional methods for SADs models
  (see \code{\link{fitsadC-class}} and examples).
  
  For counts of species in abundances classes the likelihood function is
  
  \deqn{L(\theta) = \sum^C n_i \ln P_i}{L(theta)=sum(n_i * ln P_i)} for
  i = 1, 2, 3, \ldots C, where C is the number of abundance classes,
  \eqn{n_i}{n_i} is the number of species in abundance class i.
  
  \eqn{P_i}{P_i} is the probability attributed by the distribution model
  to the observation of one species in class i, which depends on the
  vector \eqn{\theta}{theta} of free parameters of the distribution model:
  
  \deqn{P_i = \int_{L_i}^{U_i} F(x \mid \theta) dx}{P_i = \integral_from(L_i)_to(U_i) F(x|theta) dx}
  
  where F(x|theta) is the value of the probability density function for a
  cover value x, under parameter values fixed at theta, and L_i and U_i
  are the lower and upper limits of the cover class i.
  
  See \code{\link{fitsad}} for descriptions of each distribution model.
}
  
\references{
  
  Bolker, B. and R Development Core Team 2012. bbmle: Tools for
  general maximum likelihood estimation. R package version 1.0.5.2.
  http://CRAN.R-project.org/package=bbmle
  
  Chapman, D. G. 1956. Estimating the parameters of a truncated gamma
  distribution. \emph{The Annals of Mathematical Statistics, 27(2)}:
  498--506.
  
  Magurran, A.E. 1989. \emph{Ecological diversity and its measurement}.
  Princenton University Press.
  
  Preston, F.W. 1948. The commonness and rarity of species.
  \emph{Ecology 29}: 254--283.
  
  Rinne, H. 2009. \emph{The Weibull distribution: a handbook}. CRC Press
  
}

\source{all fitting functions builds on \code{mle2} and methods
  from \pkg{bbmle} package (Bolker 2012), which in turn builds on
  \code{mle} function and associated classes and methods.
}

\seealso{ \code{\link{dpareto}},for corresponding density functions created for
  fitting SADs; standard distributions \code{dexp}, \code{dgamma},
  \code{dlnorm}, \code{dweibull};
  \code{\link{fitsadC-class}}.  }

\examples{
## An example using data from Vieira et al, see dataset "grasslands"
## Breakpoints of the abundance classes used (cover classes)
vieira.brk <- c(0,1,3,5,seq(15,100, by=10),100)
##  creates an histogram object 
grass.h <- hist(grasslands$mids, breaks = vieira.brk, plot = FALSE)
#Fits Pareto, lognormal and gamma distributions
grass.p <- fitparetoC(grass.h)
grass.l <- fitlnormC(grass.h)
grass.g <- fitgammaC(grass.h)
## Predicted values by each model
grass.p.pred <- coverpred(grass.p)
grass.l.pred <- coverpred(grass.l)
grass.g.pred <- coverpred(grass.g)
## model selection
AICctab(grass.p, grass.l, grass.g, weights =TRUE, base = TRUE)
## A histogram with the densities predicted by each model
plot(grass.h, main = "", xlab = "Abundance (cover)")
## Adds predicted densities by each model to the plot
points(grass.p.pred, col = 1)
points(grass.l.pred, col = 2)
points(grass.g.pred, col = 3)
legend("topright", legend=c("Pareto","Log-normal", "Gamma"), col = 1:3, lty=1, pch =1) 
}

\author{Paulo I Prado \email{prado@ib.usp.br}, Murilo Dantas Miranda and
  Andre Chalom, after Ben Bolker, R Core Team.}

\encoding{UTF-8}
