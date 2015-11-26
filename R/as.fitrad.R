drad <- function(f, x, log = FALSE, parms) {
  rad.tab <- rad(x)
  y <- rep(rad.tab$rank, rad.tab$abund)
  p <- ppoints(length(x))
  qf <- get(paste("q", f, sep=""), mode = "function")
  num <- do.call(qf, c(list(p=p[y], log=FALSE, lower.tail=FALSE), parms))
  den <- do.call(qf, c(list(p=p, log=FALSE), dots))
  if (log) 
    log(num) - log(sum(den))
  else 
    num / sum(den)
}

prad <- function () {}

qrad <- function () {}

#' fitsad to fitrad conversion
#' 
#' Converts a \code{\link{fitsad}} object into a \code{\link{fitrad}} object, for comparisons
#'
#' This function takes a fitsad object and generates the corresponding fitrad, in order to enable
#' comparisons such as the AIC.
as.fitrad <- function(object) {
  if(!inherits(object, "fitsad")) stop("object should be a fitsad")
  sum(drad (object@sad, object@data$x, log=TRUE, object@fullcoef))

}

