\name{dpoix}
\alias{dpoix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Compound Poisson-Exponential Distribution
}
\description{
Density for the Poisson-exponential compound probability distribution.
}
\usage{
dpoix(y, frac, rate, trunc=0, log=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
vector of (non-negative integer) quantiles.
}
\item{frac}{
  single numeric '0<frac<=1'; fraction of the population or community sampled (see details)
}
\item{rate}{
  vector of (non-negative) rates of the exponential distribution of the
  sampled population (see details).
}
\item{trunc}{
  The most used option is to truncate at zero, since species with zero abundance usually are not known.
}
\item{log}{
  logical; if TRUE, probabilities p are given as log(p)
}
}
\details{
A compound Poisson-exponential distribution is a Poisson probability distribution
where its single parameter lambda, is frac*n, where n
is a random variable with exponential distribution. Thus, the expected value and variance are E[X] =
Var[X] = frac*n . The density function is

p(y) = rate*frac^y / (frac + rate)^(y+1)

(Green & Plotkin 2007). For x = 0, 1, 2, ... . In ecology, this distribution gives the probability that a species has
an abundance of x individuals in a random sample of a fraction 'frac' of the community. In the community the species abundances are independent
random variables that follow an exponential density function.

Hence, a Poisson-exponential distribution is a model for species
abundances distributions (SAD) in a sample taken from a community under the assumptions that (a) species
abundances in the community are independent identically distributed
exponential variables, (b) sampling is a Poisson proccess with expected
value 'frac*n', (c) individuals are sampled with replacement, or the
fraction of total individuals sampled is small enough to approximate a
sample with replacement. See Engen (1977) and Alonso et al. (2008) for critic evaluations.

}
\value{
(log) density of the (zero-truncated) density
}
\references{
  Alonso, D. and Ostling, A., and Etienne, R. S. 2008 The implicit
  assumption of symmetry and the species abundance
  distribution. \emph{Ecology Letters, 11}: 93-105.

  Engen, S. 1977. Comments on two different approaches to the analysis
  of species frequency data. \emph{Biometrics, 33}: 205-213.

  Pielou, E.C. 1977. \emph{Mathematical Ecology}. New York: John Wiley
  and Sons.
  
  Green,J. and Plotkin, J.B. 2007 A statistical theory for sampling species abundances. \emph{Ecology Letters 10}:1037--1045
}
\author{
Paulo I. Prado & Cristiano Strieder
}
\note{
z
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
dexp, dpois
}

\examples{
is.wholenumber <-
    function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
is.wholenumber(1) # is TRUE
(x <- seq(1,5, by=0.5) )
is.wholenumber( x ) #-->  TRUE FALSE TRUE ...
}