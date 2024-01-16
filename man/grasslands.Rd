\name{grasslands}
\docType{data}
\alias{grasslands}
\title{Coverages of plants species in a plot in Southern Brazilian Grasslands}

\description{Coverage class of each plant species recorded in one of
  the plots set in grassland in Southern Brazil ('Pampa') by Vieira &
  Overbeck (2020). }

\usage{
data(grasslands)
}

\format{ A data-frame with one plant species per row, and three vectors:
  \describe{
    \item{class}{cover class as coded in the original data
      set.}
    \item{cover}{interval of the cover class for each plant species. Cover
  is the proportion of the area of the plot covered by all individuals
  of each species}
     \item{upper}{Upper limit of the cover class of each
       plant species}
     \item{mids}{Mid-point of the cover class for each plant sepcies}
   }
 }


\source{
  Vieira & Overbeck (2020) provide cover data for each individual species in
  the whole set of eighty plots of 1 m2. The data frame \code{grasslands}
  corresponds to data from plot 'CA8', which has the largest number of
  species recorded among these plots. 
}


\references{
  Vieira, Mariana; Overbeck, Gerhard (2020). Small seed bank
  in grasslands and tree plantations in former grassland sites in the
  South Brazilian highlands
  [Dataset]. Dryad. https://doi.org/10.5061/dryad.n2z34tmsw
}

\keyword{datasets}
