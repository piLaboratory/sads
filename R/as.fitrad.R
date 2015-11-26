#' fitsad to fitrad conversion
#' 
#' Converts a \code{\link{fitsad}} object into a \code{\link{fitrad}} object, for comparisons
#'
#' This function takes a fitsad object and generates the corresponding fitrad, in order to enable
#' comparisons such as the AIC.
#' @param object A \code{link{fitsad}} object to be converted
#' @examples
#' f1 <- fitlnorm(moths)
#' logLik(f1)
#' # This is incomparable with fitrad objects, such as
#' fz <- fitzipf(moths)
#' logLik(fz)
#' # However, the converted fitrad has a comparable log-likelihood
#' f2 <- as.fitrad(f1)
#' logLik(f2)
as.fitrad <- function(object) {
  if(!inherits(object, "fitsad")) stop("object should be a fitsad")
  if (object@sad == "lnorm") {
    LL <- function(meanlog, sdlog) -sum(drad(object@sad, object@data$x, log=TRUE, list(meanlog=meanlog, sdlog=sdlog), NaN))
    result <- do.call("mle2", list(LL, eval.only=TRUE, start=list(meanlog=object@coef[1], sdlog=object@coef[2])))
  } else if(object@sad == "ls") {
    LL <- function(N, alpha) -sum(drad(object@sad, object@data$x, log=TRUE, list(N=N, alpha=alpha), NaN))
    result <- do.call("mle2", list(LL, eval.only=TRUE, start=list(N=object@fullcoef[1], alpha=object@fullcoef[2])))
  } else stop ("this distribution has not been implemented yet")

  new("fitrad", result, rad=object@sad, distr = distr.depr, trunc = object@trunc, rad.tab=rad(object@data$x))
}

drad <- function(f, x, log = FALSE, coef, trunc = NaN) {
  rad.tab <- rad(x)
  y <- rep(rad.tab$rank, rad.tab$abund)
  p <- ppoints(length(x))
  if (is.nan(trunc)) {
    qf <- get(paste("q", f, sep=""), mode = "function")
    den <- do.call(qf, c(list(p=p, log=FALSE, lower.tail=FALSE), coef))
    num <- den[y]
  } else {
    ####### TODO
    stop("truncated distributions not implemented")
  }
  if (log) 
    log(num) - log(sum(den))
  else 
    num / sum(den)
}
