\name{dpoilog}
\alias{dpoilog}
\alias{ppoilog}
\alias{qpoilog}
\title{Poisson-lognormal distribution
}
\description{
Density, distribution function and quantile function for
Poisson-longnormal distribution with parameters \code{mu} and \code{sig}.
}
\usage{
dpoilog( x, mu, sig, log=FALSE)
ppoilog( q, mu, sig, lower.tail=TRUE, log.p=FALSE)
qpoilog( p, mu, sig, S = 30, lower.tail = TRUE, log.p = FALSE)
}

\arguments{
  \item{x}{
    vector of (non-negative integer) quantiles. Usually a vector of abundances of species in a sample.
  }
  \item{q}{vector of (non-negative integer) quantiles. Usually a vector of
    abundances of species in a sample.
  }
  \item{p}{
    vector of probabilities.
  }
  \item{mu,sig}{
    parameters of the compounding lognormal distribution (see details).
  }
  \item{log, log.p}{
    logical; if TRUE, probabilities p are given as log(p).
  }
  \item{lower.tail}{
    logical; if TRUE (default), probabilities are P[X <= x],
    otherwise, P[X > x].
  }
  \item{S}{positive integer; initial value for bissection method.
  (see note)
    }
}

\details{
 A compound Poisson-lognormal distribution is a Poisson probability distribution
where its single parameter lambda, is a random variable with lognormal
 distribution. 
The density function is 

p(x) = (exp(x*mu + x^2*sig/2)*(2*pi*sig)^(-1/2))/x! * g(y)

where g(y) =
int_-infty^infty exp(-exp(y))*exp(((-y-mu-x*sig)^2)/(2*sig)) dy

(Bulmer 1974 eq.5). For x = 0, 1, 2, ... .

In ecology, this distribution gives the probability that a species has
an abundance of x individuals in a random sample of a fraction 'f' of the
community. In the community the species abundances are independent
random variables that follow a lognormal density function, with
parameters (mu + ln(f), sigma) (Engen et al. 2002).

Hence, a Poisson-exponential distribution is a model for species
abundances distributions (SAD) in a sample taken from a community
under the assumptions that (a) species
abundances in the community are independent identically distributed
exponential variables, (b) sampling is a Poisson proccess with expected
value E[x]= f*n where n is the abundance in the community and f the
fraction of individuals sampled, (c) individuals are sampled with replacement, or the
fraction of total individuals sampled is small enough to approximate a
sample with replacement. See Engen (1977) and Alonso et al. (2008) for critical evaluations.
}

\value{
'dpoilog' gives the (log) density of the density, 'ppoilog' gives the (log)
distribution function, 'qpoilog' gives the quantile function.
}

\note{
parameter \code{S} in \code{qpoilog} is just a starting value for the
bissection method, and does not affect the final result, only the
convergence velocity. A good guess is the total number of observations,
which in the context of sads is the observed number of species.
}

\source{
  The three functions were built from \code{dpoilog} function from \pkg{poilog}
  package (Vidar \enc{Grøtan}{Grotan} and Steinar Engen).
  
  \code{dpoilog} is just a wraper of \code{poilog::dpoilog} with an additional 'log' argument.

  \code{ppoilog} does the cumulative sum of \code{poilog::dpoilog}.

  \code{qpoilog} uses modified bissection method to find numerically quantiles using
  \code{ppoilog}, and is extremely slow for a large number of quantiles.
}

\references{
  
  Alonso, D. and Ostling, A., and Etienne, R. S. 2008 The implicit
  assumption of symmetry and the species abundance
  distribution. \emph{Ecology Letters, 11}: 93-105.

  Bulmer,M. G. 1974.On Fitting the Poisson Lognormal Distribution to
  Species-Abundance Data. \emph{Biometrics, 30}: 101-110.

  Engen, S. 1977. Comments on two different approaches to the analysis
  of species frequency data. \emph{Biometrics, 33}: 205-213.
  
  Engen, S., R. Lande, T. Walla & P. J. DeVries. 2002. Analyzing spatial
  structure of communities using the two-dimensional Poisson lognormal
  species abundance model. \emph{American Naturalist 160}: 60-73.
}

\author{
Paulo I. Prado \email{prado@ib.usp.br} and Murilo Dantas Miranda
}

\seealso{
dpois, dlnorm; dpoilog and rpoilog in poilog package; \link{\code{rsad}} for random
generation, \link{\code{fitpoilog}} for maximum likelihood estimation.
}
