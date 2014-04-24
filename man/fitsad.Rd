\name{fitsad}
\alias{fitsad}
\alias{fitbs}
\alias{fitgamma}
\alias{fitgeom}
\alias{fitlnorm}
\alias{fitls}
\alias{fitmzsm}
\alias{fitnbinom}
\alias{fitpareto}
\alias{fitpoilog}
\alias{fitpower}
\alias{fitvolkov}
\alias{fitweibull}

\title{ML fiting of species abundance distributions}

\description{Fits probability distributions for
  abundances of species in a sample or assemblage by maximum likelihood.
}

\usage{
fitsad(x,
sad =
  c("bs",
  "gamma","geom","lnorm","ls","mzsm","nbinom","pareto","poilog","power","weibull","volkov"), ...)

fitbs(x, trunc, ...)

fitgamma(x, trunc, start.value, trueLL = TRUE, dec.places = 0, ...)

fitgeom(x, trunc = 0, start.value, ...)

fitlnorm(x, trunc, start.value, trueLL = TRUE, dec.places = 0, ...)

fitls(x, trunc, start.value, upper = length(x), ...)

fitmzsm(x, trunc, start.value, upper = length(x), ...)

fitnbinom(x, trunc, start.value, ...)

fitpareto(x, trunc, start.value, trueLL = TRUE, dec.places = 0, upper = 20, ...)

fitpoilog(x, trunc = 0, ...)

fitpower(x, trunc, start.value, upper = 20, ...)

fitvolkov(x, trunc, start.value, ...)

fitweibull(x, trunc, start.value, trueLL = TRUE, dec.places = 0, ...)
}

\arguments{
  \item{x}{vector of (positive integer) quantiles. In the context of
    sads, some abundance measurement (e.g., number of individuals,
    biomass) of species in a sample or ecological assemblage.
  }
  \item{sad}{
    character; root name of community sad distribution to be fitted.
    "gamma" for gamma distribution,
    "geom" for geometric distributions (not geometric series rad model,
    \link{dgs}),
    "lnorm" for logmormal,
    "ls" for Fisher's logseries,
    "mzsm" for Alonso & McKane's neutral metacommunity distribution,
    "nbinom" for negative binomial,
    "pareto" for Pareto distribution,
    "poilog" for Poisson-lognormal distribution,
    "power" for power-law distribution,
    "volkov" for Volkov's et al. neutral community distribution,
    "weibull" for Weibull distribution.
  }

  \item{trunc}{
    non-negative integer, trunc > min(x); truncation point to fit a
    truncated distribution.
  }

  \item{start.value}{
    numeric named list; starting values of free parameters to be
    passed to \code{mle2}.
    Parameters should be named as in the corresponding density
    function,
    and in the same order.
  }
  
  \item{trueLL}{
    logical; use 'true likelihood' for continuous models? (see \link{trueLL}).
  }

  \item{dec.places}{
    positive integer; number of decimal places used in the measurement of
    the observed values. Observed values will be rounded to this number of
    decimals. This argument defines the measurement precision for
    abundance variables. Used for continuous distributions when
    argument \code{trueLL=TRUE} 
    (see details and \link{trueLL}).
  }

  \item{upper}{
    real positive; upper bound for the Brent's one-parameter optimization
    method (default), for fits that use this method by default. See
    details and \link{optim}.
  }
  \item{...}{
    in fitsad further arguments to be passed to the specific fitting
    function (most used are \code{trunc}, \code{start.value} and
    \code{trueLL}). 
    In the specific fitting functions further
    arguments to be passed to \code{mle2}.
    }
  }
  
\value{
  An object of class \code{fitsad} which inherits from \code{mle2}
  class and thus has methods for handling
  results of maximum likelihood fits from \code{mle2}
  and also specific methods to handle SADs models
  (see \link{fitsad-class}).
}

\details{
  \code{fitsad} is simply a wraper that calls the specific functions to fit
  the distribution chosen with the argument \code{sad}. Users
  can interchangeably use \code{fitsad} or the individual functions
  detailed below 
  (e.g. \code{fitsad(x, sad="geom", ...)} is the same as
  \code{fitgeom(x, ...)} and so on).
  
  The distributions are fitted by the
  maximum likelihood method using numerical optimization,
  with \code{mle2}.
  The result object is of new class \code{fitsad}
  which can be handled with \code{mle2} methods
  for fitted models and has also some additional
  methods for SADs models (see
  \link{fitsad-class} and examples).
  
  Functions \code{fitgamma}, \code{fitlnorm}, \code{fitweibull}, fit the
  standard continuous distributions most used as SADs models.
  Functions \code{power} and \code{pareto} fit power-law continuous distributions
  with one and two-parameters, that have been suggested as SADs models.
  By default, all continuous distributions
  are fit using the
  correct likelihood for independent observations of a
  continuous variable (Lindsey 1999, see \link{trueLL}).
  Hence, the correct number of decimal places of the abundance
  measure must be given in the argument \code{dec.places}.
  A warning message reminds users about that when \code{trueLL=TRUE}.
  When the abundance measure is the number of individuals of each
  species in the sample,
  the default value \code{dec.places=0}
  is the correct choice. In this case, the probability of a given abundance
  value x will be calculated as CDF(x + 0.5) - CDF( x - 0.5 ), where CDF
  is the cumulative density function of the continuous distribution.
  
  Functions \code{fitgeom}, \code{fitnbinom} are two discrete
  standard distributions also uded to fit SADs. Since species
  with zero individuals in the sample are in general unknown,
  these functions fit by default zero-truncated distributions.
  Using the geometric distribution as a SAD model is not to be
  confounded to fitting the Geometric series \link{fitgeom}
  as a rank-abundance
  distribution (RAD) model.

  Function \code{fitls} implements the original numerical recipe by Fisher (1943) to
  fit the logseries distribution, given a vector of species abundances.
  Alonso et al. (2008,
  supplementary material) showed that this recipe gives the maximum
  likelihood estimate of Fisher's alpha, the single parameter of the logseries.
  Fitting is done through numerical optimization with the \code{uniroot}
  function, following the code of the function \code{fishers.alpha} of the
  'untb' package. After that, fitted value of the alpha parameter is
  use to get the Log-likelihood from the logseries density
  function \code{dls}, using the function \code{mle2}.

  Function \code{fitbs} fits the Broken-stick distribution
  (MacArthur 1960). It is defined only by the observed number of
  elements S in the collection and collection size N.
  Therefore, once a sample is taken,
  the Broken-stick has no free parameters.
  Therefore, there is no actual "fitting", but still
  the \code{fitbs} calls
  \code{mle2} with
  fixed parameters N and S and \code{eval.only=TRUE}
  to return an object of classes \code{fitsad}
  or \code{fitrad} to keep compatibility with other
  sad and rad models fitted to the same data.
  Therefore the resulting objects allows most of the
  operations with sad or rad models, such as
  comparison with other models through model selection,
  diagnostic plots and so on
  (see \link{fitsad-methods}).

  Function \code{fitpoilog} fits the Poisson-lognormal distribution.
  This is a compound distributions that describes the abundances
  of species in Poisson sample of community that follows a
  lognormal SAD. This is a sampling model of SAD, which is
  approximated by the 'veil line' truncation of the lognormal
  (Preston (1948)). The Poisson-lognormal is the analytical
  solution for this sampling model, as Fisher's logseries
  is a analytical limit case for a Poisson-gamma
  (a.k.a negative binomial) distribution. As geometric and
  negative binomial distributions, the Poisson-lognormal
  includes zero, but the fit is zero-truncated
  by default, as for \code{fitgeom}, \code{fitnbinom}.
  
  \code{fitmzsm} fits the metacommunity Zero-sum multinomial distribution
  \link{dmzsm} from the Neutral Theory of Biodiversity
  (Alonso and McKane 2004). The mZSM describes the SAD of a sample
  taken from a neutral metacommunity under random drift.
  It has two parameters,
  the number of individuals in the sample (J) and theta,
  the 'fundamental biodiversity number'.
  Because J is known from the sample size,
  the fit resumes to estimate a single
  parameter, theta. By default, \code{fitmzsm} fits mZSM to a vector of abundances
  with Brent's one-dimensional method of optimization (see
  \link{optim}). The logseries distribution (Fisher et al. 1943)
  is a limiting case of mZSM (Hubbel 2001), and theta tends to
  Fisher's alpha as J increases. In practice
  the two models provide very similar fits to SADs (see example).

  Function \code{fitvolkov} fits the SAD model for a community
  under neutral drift with immigration
  (Volkov et al. 2003).
  The model is a stationary distribution deduced from
  a stochastic process compatible with the Neutral Theory
  of Biodiversity (Hubbell 2001). It 
  has two
  free parameters, the 'fundamental biodiversity number' theta, and the
  immigration rate m (see \link{dvolkov})
  \code{fitvolkov} builds on \code{untb::volkov} function to
  fit Volkov's et al SAD model to a vector of abundances.
  The fit can be extremely slow even for vectors
  of moderate size.
}

\references{

  Alonso, D. and McKane, A.J.  2004.  Sampling Hubbell's neutral
  model of biodiversity. \emph{Ecology Letters 7}:901--910

  Alonso, D. and Ostling, A., and Etienne, R.S. 2008 The implicit
  assumption of symmetry and the species abundance
  distribution. Ecology Letters, 11: 93-105.

  Bolker, B. and R Development Core Team 2012. bbmle: Tools for
  general maximum likelihood estimation. R package version 1.0.5.2.
  http://CRAN.R-project.org/package=bbmle

  Fisher, R.A, Corbert, A.S. and Williams, C.B. (1943) The Relation
  between the number of species and the number of individuals in a
  random sample of an animal population.
  \emph{The Journal of Animal Ecology, 12(1)}: 42--58.

  Grotan, V. and Engen, S. 2008. poilog: Poisson lognormal and
  bivariate Poisson lognormal distribution. R package version 0.4.
  
  Hankin, R.K.S. 2007. Introducing untb, an {R} Package For Simulating Ecological Drift
  Under the Unified Neutral Theory of Biodiversity. \emph{Journal
    of Statistical Software 22} (12). 
  
  Hubbell, S.P.  2001.  \emph{The Unified Neutral Theory of Biodiversity}.
  Princeton University Press
  
  Lindsey, J.K. 1999. Some statistical heresies. \emph{The Statistician
    48}(1): 1--40.
  
  MacArthur, R.H. 1960. On the relative abundance of species.
  \emph{Am Nat 94}:25--36.
  
  Magurran, A.E. 1989. \emph{Ecological diversity and its measurement}.
  Princenton University Press.
  
  Preston, F.W. 1948. The commonness and rarity of species.
  \emph{Ecology 29}: 254--283.

  Volkov, I.,  Banavar, J. R., Hubbell, S. P., Maritan, A. 2003.
  Neutral theory and relative species abundance in ecology.  \emph{Nature 424}:1035--1037.
}

\source{all fitting functions builds on \code{mle2} and methods
  from 'bbmle' package (Bolker 2012), which in turn builds on
  \code{mle} function and associated classes and methods;
  \code{fitls} and \code{fitvolkov} uses codes and functions from
  'untb' package (Hankin 2007); \code{fitpoilog} builds on
  'poilog' package (Grotan & Engen 2008).
}

\seealso{
  \link{dls}, \link{dmzsm}, \link{dpareto},
  \link{dpoilog}, \link{dpower}, \link{dvolkov}
  for corresponding density functions created for fitting SADs;
  standard distributions \code{dweibull},
  \code{dgamma}, \code{dgeom}, \code{dlnorm}, \code{dnbinom};
  \link{fitsad-class}.
}

\examples{

## Magurran (1989) example 5:
## birds in an Australian forest
mag5 <- c(103,115,13,2,67,36,51,8,6,61,10,21,
          7,65,4,49,92,37,16,6,23,9,2,6,5,4,
          1,3,1,9,2)
mag5.bs <- fitsad(mag5, "bs") 
summary(mag5.bs)## no estimated coefficient
coef(mag5.bs) ## fixed coeficients N and S
## Diagnostic plots
par(mfrow=c(2,2))
plot(mag5.bs)
par(mfrow=c(1,1))


data(moths) #Fisher's moths data
moths.mzsm <- fitmzsm(moths) ## same as fitsad(moths, sad="mzsm")
## fit to logseries
moths.ls <- fitsad(moths, sad="ls")
coef(moths.ls)
coef(moths.mzsm) ## Compare with theta=38.9, Alonso&McKanne (2004)
## Diagnostic plots
par(mfrow=c(2,2))
plot(moths.mzsm)
par(mfrow=c(1,1))
## Graphical comparison
plot(rad(moths))
lines(radpred(moths.ls))
lines(radpred(moths.mzsm), col="red", lty=2)
legend("topright", c("log-series", "mZSM"), lty=1, col=c("blue","red"))
## Two more models: truncated lognormal and Poisson-lognormal
moths.ln <- fitsad(moths, "lnorm", trunc=0.5)
moths.pln <- fitsad(moths, "poilog")
## Model selection
AICtab(moths.ln, moths.pln, moths.ls, moths.mzsm, weights=TRUE)

## Biomass as abundance variable
data(ARN82.eB.apr77) #benthic marine animals
AR.ln <- fitsad(ARN82.eB.apr77, sad="lnorm", dec.places=2)
AR.g <- fitsad(ARN82.eB.apr77, sad="gamma", dec.places=2)
AR.wb <- fitsad(ARN82.eB.apr77, sad="weibull", dec.places=2)                
plot(octav(ARN82.eB.apr77))
lines(octavpred(AR.ln))
lines(octavpred(AR.g), col="red")
lines(octavpred(AR.wb), col="green")
legend("topright", c("lognormal", "gamma", "weibull"), lty=1, col=c("blue","red", "green"))
AICctab(AR.ln, AR.g, AR.wb, nobs=length(ARN82.eB.apr77), weights=TRUE) 
}

\author{Paulo I Prado \email{prado@ib.usp.br} and Murilo Dantas Miranda}

