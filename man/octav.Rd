\name{octav}

\alias{octav}

\title{Frequencies of species in octaves}

\description{Creates an object of \code{octav-class} with number of
  species in octaves of abundances from a vector
  of abundances or from a fitted model.
}

\usage{octav(x, oct, preston=FALSE)}

\arguments{
  
  \item{x}{a numerical vector of abundances or an object of class
    \code{fitsad} or \code{fitrad}.}
  \item{oct}{integer vector; the octaves to tabulate abundances. Should
    include all abundance values in \code{x}.
  }
  \item{preston}{logical; if 'TRUE' use Preston method to count
    frequencies (see details), if 'FALSE' class intervals are open on
    the left (default in \code{cut}).
  }
}

\value{
  an object of class \code{octav}, which is a data frame with three vectors:
  \item{octav}{integer; octave number, which is the upper
    limit of the class in log2.}
  \item{upper}{numeric; upper limit of the class in arithmetic
    scale.}
  \item{Freq}{integer or numeric; (relative) frequencies of species in
    each class.}  
}

\details{Preston (1948) popularized the use of histograms with
  logarithmic classes to depict species abundance distributions
  (Magurran 1989). Preston used classes at log base two, which he called
  \sQuote{octaves} as their end-points double from one class to the other. In
  Preston original method half of the species with abundances equal to the
  limits of octaves are credited to the neighboring octave. If
  \code{preston=TRUE} this non-standard method of class closure is
  applied. In general this makes the histogram more
  bell-shaped, as Preston expected (see example). }

\references{
  Magurran, A.E. 1989. \emph{Ecological diversity and its measurement}.
  Princeton University Press.
  
  Preston, F.W. 1948. The commonness and rarity of species.
  \emph{Ecology 29}: 254--283.
}

\seealso{ \code{\link{octav-class}} for methods to create an octave
  plot; \code{\link{octavpred}} to get an \code{octav} object of
  predicted abundances from a theoretical distribution;  \code{\link{fitsad-class}} and
  \code{\link{fitrad-class}} objects, from which you can also get an
  object of class \code{octav}; man page of \code{prestonfit} in package \pkg{vegan} for a
  detailed account of Preston's octaves and an alternative way to get
  octaves and fitting of species abundances distributions.  
}

\examples{
## BCI tree data
(bci.oc1 <- octav(bci, preston=TRUE))
## Comparing with standard class closure
par(mfrow=c(1,2))
plot(octav(bci), main="octav(bci, preston=FALSE)")
plot(bci.oc1, main="octav(bci, preston=TRUE)")
par(mfrow=c(1,1))
}

\author{
  Paulo I Prado \email{prado@ib.usp.br} and Murilo Dantas Miranda
}


