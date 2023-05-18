\name{kansas}
\docType{data}
\alias{kansas}
\title{Coverages of plants species in a plot in Kansas prairie}

\description{Coverage of each plant species recorded in one of
  the permanet plots set in a mixed prairie in Kansas. Abundances are
  recorded as area covered (cm2) proportion of the plot area
  covered by each plant species, and also in Londo's scale of coverage.}

\usage{
data(kansas)
}

\format{
  A data-frame with three vectors:
  \describe{
    \item{area.cm2}{area of the plot covered by all individuals of each species in
      square centimeters}
    \item{cover}{proportion of the area of the plot covered by all
      individuals of each species}
    \item{londo}{Upper limit of the coverage class in decimal scale
  (Londo, 1976)}
    }
}


\source{
  Adler et al. (2007) provide cover data for each individual species in
  the whole set of plots along 1932-1972. The data frame \code{kansas}
  was built by aggregating cover values by species for a single plot in a single
  year (variable 'plotyear' in the original data ==
  "e1q4-164"). Relative cover and cover at decimal scale were then calculated.
}


\references{
  Londo, G. (1976). The decimal scale for releves of
  permanent quadrats. \emph{Vegetatio 33:}, 61-64.

  Adler, P.B., Tyburczy,
  W.R., and Lauenroth, W.K. (2007). Long-term mapped quadrats from
  Kansas prairie: demographic information for herbaceaous plants.
  \emph{Ecology 88:}: 2673.  
}

\keyword{datasets}
