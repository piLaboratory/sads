\name{pred.logser}
\alias{pred.logser}
\title{Predicted number of species of Fisher's Logseries}
\description{
Given a vector of species abundances, Fisher's alpha and total number of
species and individuals in a sample, returns the number of
species for each abundance value expected by the Fisher's logseries model
}
\usage{
pred.logser(x, alpha, J, S)
}

\arguments{
  \item{x}{
    Vector of (non-negative integer) abundances of species in a sample.
  }
  \item{alpha}{
    Fisher's alpha
  }
  \item{J}{
    Total number of individuals in the sample.
  }
  \item{S}{
    Total number of species in the sample.
  }
}

\details{

  The Fisher logseries is a limiting case of the Negative Binomial where
  the dispersion parameter of the negative binomial tends to zero. It
  was originally proposed by Fisher (1943) to relate the expected number
  of species in a sample from a biological community to the sample size as:

  S = alpha * log(1 + J/alpha)

  Where alpha is the single parameter of the logseries distribution,
  often used as a diversity index. From this relation follows that the
  expected number of species with x individuals in the sample is
  
  S(x) = alpha*X^x/x

  Where X is a function of alpha and J, that tends to one as the sample
  size J increases:

  X = J / (alpha + J)
  
  Since the logseries model is a function that relates S to J using a
  single parameter (alpha), once two of
  these quantities are known the remaining is determined. So the function
  allow the input any two among S, J and alpha. If the
  user does not provide at least two of these values, an error message
  is returned.
  
  This function returns the expected number of species with abundance x,
  which is
  
  E[S(x)] = x^(-1)*alpha*X^x

}

\value{
  A (vector) of expected number of species to each abundance provided by
  argument \code{x}
}

\references{
  
  Pielou, E.C. 1977. \emph{Mathematical Ecology}. New York: John Wiley
  and Sons.

  Fisher, R.A, Corbert, A.S. and Williams, C.B. (1943) The Relation
  between the number of species and the number of individuals in a
  random sample of an animal population. \emph{The Journal of Animal Ecology, 12(1)}: 42--58.
}

\author{
Paulo I. Prado
}

\seealso{
dpoig, dls, uniroot, fit.logser, fishers.alpha in package untb and fisherfit
in package vegan. 
}

\examples{
data(moths) # Willians' moth data
pred.logser(1:5, J=sum(moths), S=length(moths)) #predicted
table(moths)[1:5] # observed 
}