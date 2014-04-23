\name{dvolkov}

\alias{dvolkov}
\alias{pvolkov}
\alias{qvolkov}

\title{Neutral Biodiversity Theory distribution by Volkov et al with
  parameters m and theta}

\description{Density, distribution function and quantile function
  for species abundances distribution in a neutral community with
  immigration as deduced by Volkov et al 2003.
}

\usage{
dvolkov( x, theta, m, J, log = FALSE )
pvolkov( q, theta, m , J, lower.tail = TRUE, log.p = FALSE )
qvolkov( p, theta, m, J, lower.tail = TRUE, log.p = FALSE )
}

\arguments{
  
    \item{x}{
      vector of (non-negative integer) quantiles. In the context of
      species abundance distributions, this is a
      vector of abundance of species in a sample.
    }

    \item{q}{vector of (non-negative integer) quantiles. In the context of
      species abundance distributions, a vector of abundance of species in a sample.
    }
    \item{p}{
      vector of probabilities.
    }

    \item{theta}{
      positive real, theta > 0; Hubbell's 'fundamental biodiversity number'
    }


    \item{m}{
      positive real, 0 <= m <= 1; immigration rate (see details)
    }
    
    \item{J}{
      positive integer 0 < J < Inf, sample size.  In the context of
      species abundance distributions, usually the number of individuals in a sample.
    }
    
    \item{log}{
      logical; if TRUE, probabilities p are given as log(p).
    }
    
    \item{lower.tail}{
      logical; if TRUE (default), probabilities are P[X <= x],
      otherwise, P[X > x].
    }
    
  }
  
  \value{
    \code{dvolkov} gives the (log) density of the density, \code{pvolkov} gives the (log)
    distribution function, \code{qvolkov} gives the (log) the quantile function.
    
    Invalid values for parameters \code{J} or \code{theta} will result in return
    values \code{NaN}, with a warning.
  }
  
  \details{
    Volkov et al (2003) proposed one of the analytic solutions for the
    species abundance distributions (SADs)
    for The Neutral Theory of Biodiversity (Hubbell 2001).

    The solution is deduced from a model of stochastic dynamics of a
    set of species where the following rules apply:
    (1) replacement of a dead individual by local offspring -- with probability 1-m,
    individuals picked at random
    are replaced by the offspring of other individuals picked at random;
    (2) replacement of a dead individual by an immigrant --
    with probability m individuals picked at random
    are replaced by an immigrants taken at random from a pool
    of potential colonizers (the metacommunity).

    Volkov et al. (2003, eq.7) provide the stationary solution for
    the expected number
    of species with a given abundance.
    A probability density function is easily calculated by taking
    these expected values for abundances 1:J and dividng them
    by the total number of
    species. \code{dvolkov} is a wraper of \code{untb::volkov} (Hankin
    2007) that
    performs these calculations. \code{pvolkov} provides CDF by
    cumulative sum of density values, and \code{qvolkov} use
    a numeric interpolation with a step function (\code{approxfun})
    to find quantiles.
    Calculations can be slow even for datasets of moderate size.
  }
  
  \references{

    Hankin, R. K. S. 2007. Introducing untb, an {R} Package For Simulating Ecological Drift
	Under the Unified Neutral Theory of Biodiversity. \emph{Journal
	of Statistical Software 22} (12). 

    Hubbell, S. P.  2001.  \emph{The Unified Neutral Theory of Biodiversity}.
    Princeton University Press.
    
    Volkov, I.,  Banavar, J. R., Hubbell, S. P., Maritan, A. 2003.
    Neutral theory and relative species abundance in ecology.
    \emph{Nature 424}:1035--1037
  }
  
  \seealso{\link{\code{fitvolkov}} for maximum likelihood fit,
    \link{\code{dmzsm}} for the distribution of abundances in the metacommunity,
    \code{volkov} in package 'untb'.
  }
  
  \source{function \code{untb::volkov} (Robin Hankin)
    calculates the expected number of species in a given abundance class
    according to eq. 7 in Volkov et al (2003).
    \code{dvolkov} is simply a wraper that converts expected numbers to densities.}
  
  \examples{
## Volkov et al 2003 fig 1
## But without Preston correction to binning method
## (only line of Volkov's model)
data( bci )
bci.oct <- octav( bci, preston = FALSE )
plot( bci.oct )
cdf <- pvolkov( bci.oct$upper, theta = 47.226, m = 0.1, J = sum(bci) )
bci.exp <- diff( c(0,cdf) ) * length(bci)
midpoints <- as.numeric( bci.oct$octave ) - 0.5
lines( midpoints, bci.exp, type="b" )

## the same with octavpred
bci.exp2 <- octavpred( bci, sad = "volkov",
                      coef = list(theta = 47.226, m = 0.1) )
lines( bci.exp2 )

}

