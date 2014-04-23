\name{dpoilog}
\alias{dpoilog}
\alias{ppoilog}
\alias{qpoilog}
\title{Poisson-lognormal distribution
}
\description{
Density, distribution function and quantile function for
Poisson-longnormal distribution with parameters ‘mu’ and ‘sig’.
}
\usage{
dpoilog( x, mu, sig, log=FALSE)
ppoilog( q, mu, sig, lower.tail=TRUE, log.p=FALSE)
qpoilog( p, mu, sig, S=30)
}

\arguments{
  \item{x}{
    vector of (non-negative integer) quantiles. Usually a vector of abundances of species in a sample.
  }
  \item{q}{vector of (non-negative integer) quantiles. Usually a vector of
    abundances of species in a sample.
  }
  \item{p}{
    vetor of probabilities.
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
  \item{S}{
    }
}

\details{
 A compound Poisson-lognormal distribution is a Poisson probability distribution
where its single parameter lambda, is a random variable with lognormal distribution The density function is 

(exp(x*mu + x^2*sig/2)*(2*pi*sig)^(-1/2))/x! * g(y)

where g(y) =
int_-infty^infty exp(-exp(y))*exp(((-y-mu-x*sig)^2)/(2*sig)) dy

(Bulmer 1974 eq.5). For x = 0, 1, 2, ... .

In ecology, this distribution gives the probability that a species has
an abundance of x individuals in a random sample of a fraction 'f' of the
community. In the community the species abundances are independent
random variables that follow a lognormal density function, with
parameters (mu + ln(f), sigma) (Engen et al. 2002).

Hence, a Poisson-exponential distribution is a model for species
abundances distributions (SAD) in a sample taken from a community under the assumptions that (a) species
abundances in the community are independent identically distributed
exponential variables, (b) sampling is a Poisson proccess with expected
value E[x]= f*n where n is the abundance in the community and f the
fraction of individuals sampled, (c) individuals are sampled with replacement, or the
fraction of total individuals sampled is small enough to approximate a
sample with replacement. See Engen (1977) and Alonso et al. (2008) for critic evaluations.
}

\value{
'dpoilog' gives the (log) density of the density, 'ppoilog' gives the (log)
distribution function, 'qpoilog' gives the quantile function.
}

\source{
  The three functions were built from dpoilog function from poilog
  package (Vidar Grøtan and Steinar Engen).
  
  dpoilog is just a wraper of poilog::dpoilog with an additional 'log' argument.

  ppoilog does the cumulative sum of poilog::dpoilog.

  qpoilog uses the ... method to approximate numerically quantiles using poilog::dpoilog.
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
Paulo I. Prado and Murilo Dantas Miranda
}

\seealso{
dpois, dlnorm; dpoilog and rpoilog in poilog package; rsad for random
generation, fitpoilog for maximun likelihood estimation.
}
