\name{radpred-methods}
\docType{methods}
\alias{radpred}
%\alias{radpred-methods}
\alias{radpred,fitrad,missing,missing,missing,missing,missing,missing,missing,missing-method}
\alias{radpred,fitsad,missing,missing,missing,missing,missing,missing,missing,missing-method}
\alias{radpred,fitsadC,missing,missing,missing,missing,missing,missing,missing,missing-method}
\alias{radpred,missing,character,missing,list,ANY,ANY,ANY,numeric,numeric-method}
\alias{radpred,missing,missing,character,list,ANY,ANY,missing,numeric,numeric-method}
\alias{radpred,numeric,character,missing,list,ANY,ANY,ANY,missing,missing-method}
\alias{radpred,numeric,missing,character,list,ANY,ANY,missing,missing,missing-method}

\title{Predicted ranked abundance of species}

\description{
  Creates an object of \code{rad-class} with the ranked abundances
  predicted by a species abundance distribution or a rank-abundance distribution.
}

\section{Methods}{

  \describe{
    
    \item{\code{signature(object = "fitrad", sad = "missing", rad =
	"missing", coef = "missing", trunc = "missing", trunc.max =
	"missing", distr = "missing", S = "missing", N = "missing")}}{
      ranked abundances of species
      predicted from a rank-abundance model fitted with function
      \code{fitrad}.
    }
    
    \item{\code{signature(object = "fitsad", sad = "missing", rad =
	"missing", coef = "missing", trunc = "missing", trunc.max =
	"missing", distr = "missing", S = "missing", N = "missing")}}{
      ranked abundances of species
      predicted from a abundance distribution  model fitted with function
      \code{fitsad}.
    }

    \item{\code{signature(object = "fitsadC", sad = "missing", rad =
	"missing", coef = "missing", trunc = "missing", trunc.max =
	"missing", distr = "missing", S = "missing", N = "missing")}}{
      ranked abundances of species
      predicted from a abundance distribution  model fitted with function
      \code{fitsadC}.
    }

    \item{\code{signature(object = "missing", sad = "character", rad =
	"missing", coef = "list", trunc = "ANY", trunc.max = "ANY", distr =
	"ANY", S = "numeric", N = "numeric")}}{
      ranked abundances of species predicted
      from abundance distribution named by \code{sad} with parameters
      defined in \code{coef}. 
    }

    \item{\code{signature(object = "numeric", sad = "character", rad =
	"missing", coef = "list", trunc = "ANY", trunc.max = "ANY", distr =
	"ANY", S = "missing", N = "missing")}}{
      same as previous method, but with
      \code{S} and \code{N} taken from a vector of abundances given by
      \code{object}.
    }
    
    \item{\code{signature(object = "missing", sad = "missing", rad =
	"character", coef = "list", trunc = "ANY", trunc.max = "ANY", distr =
	"missing", S = "numeric", N = "numeric")}}{
      ranked abundances of species predicted from a rank-abundance
      distribution  named by \code{rad} with parameters defined in
      \code{coef}.
    }

    \item{\code{signature(object = "numeric", sad = "missing", rad =
	"character", coef = "list", trunc = "ANY", trunc.max = "ANY", distr =
	"missing", S = "missing", N = "missing")}}{
      same as previous method, but with
      \code{S} and \code{N} taken from a vector of abundances given by
      \code{object}. 
    }
}}

\arguments{
  
  \item{object}{an object of class \code{fitrad} or \code{fitsad};
    fitted model of rank-abundance or species abundances
    distributions. Alternatively a numeric vector abundances of
    species. 
  }
  \item{sad, rad}{character; root name of sad or rad distribution to
    calculate expected percentiles. See \code{\link{fitsad}} and
    \code{\link{fitrad}} for available distributions.
  }
  \item{coef}{named list of numeric values; parameter values of the
    distribution given in \code{sad} or \code{rad}.
    Parameters should be named as in the corresponding density
    function, and in the same order.
  }
  \item{trunc}{non-negative integer, trunc > min(x); lower truncation point
    if fitted distribution is left-truncated.
  }
  \item{trunc.max}{numeric, trunc.max >= max(x); upper truncation point
    if fitted distribution is right-truncated. Can be combined with
    \code{trunc} for a distribution truncated at both ends. See
    \code{\link{dtrunc}} for details.
  }
  \item{distr}{Deprecated since sads 0.2.4. See \code{\link{distr}} function}
  \item{S}{positive integer; number of species in the sample.
  }
  \item{N}{positive integer; number of individuals in the sample.
  }
}

\keyword{methods}

\note{The rank-abundance function is the inverse of the quantile
  function of the species abundance distribution (May 1975). \code{radpred}
  uses numeric interpolation with \code{approxfun} to find quantiles,
  instead of the slower bisection method used in many quantile functions in
  the package \pkg{sads}. Hence results from \code{radpred} and from the
  quantile functions may not match exactly.
}

\references{
  Alonso, D., & McKane, A.J. 2004. Sampling Hubbell's
  neutral theory of biodiversity. \emph{Ecology letters, 7}: 901--910.

  Fisher, R.A., Corbert, A.S. and Williams, C.B. 1943. The Relation
  between the number of species and the number of individuals in a random
  sample of an animal population.
  \emph{The Journal of Animal Ecology, 12}: 42--58.

  May, R.M. 1975. Patterns of Species Abundance and Diversity. In Cody,
  M.L. and Diamond, J.M. (Eds) \emph{Ecology and Evolution of
    Communities}. Harvard University Press. pp 81--120.
}

\author{
  Paulo I. Prado \email{prado@ib.usp.br} and Murilo Dantas Miranda
}

\examples{
## Predicted frequencies from a fitted model meta-community zero-sum
## multinomial for pooled samples of Moths caught with light traps at
## Rothamsted 1933-1936 (Fisher et al. 1943)
moths.mzsm <- fitsad(moths, "mzsm")
moths.mzsm.r <- radpred(moths.mzsm)
## Rank-abundance plot with observed and predicted frequencies
plot(rad(moths))
lines(moths.mzsm.r)
## Alternative model: local zero-sum multinomial
## Alonso & Mckane (Ecol. Lett. 2004, table 1) give theta = 41 and m = 0.77
moths.lzsm.r <- 
  radpred( moths, sad = "volkov",  
           coef =list(theta = 41, m = 0.77, J=sum(moths)) 
         )
## Adding predicted frequencies to the plot
lines(moths.lzsm.r, col = "red")

## Predictions from a model truncated at both a minimum and a maximum abundances
moths.ln2 <- fitsad(moths, "lnorm", trunc = 0.5, trunc.max = 2500)
moths.ln2.r <- radpred(moths.ln2)
}
