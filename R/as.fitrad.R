drad <- function(f, x, log = FALSE, coef, trunc) {
  rad.tab <- rad(x)
  y <- rep(rad.tab$rank, rad.tab$abund)
  p <- ppoints(length(x))
  if (is.nan(trunc)) {
    qf <- get(paste("q", f, sep=""), mode = "function")
    num <- do.call(qf, c(list(p=p[y], log=FALSE, lower.tail=FALSE), coef))
    den <- do.call(qf, c(list(p=p, log=FALSE), coef))
  } else {
    ####### TODO
  }
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
  if(!is.nan(object@trunc)) stop("truncated objects are not allowed in this version")
  if (object@sad == "lnorm") {
    LL <- function(meanlog, sdlog) -sum(drad(object@sad, object@data$x, log=TRUE, list(meanlog=meanlog, sdlog=sdlog), NaN))
    result <- do.call("mle2", list(LL, eval.only=TRUE, start=list(meanlog=object@coef[1], sdlog=object@coef[2])))
  } else if(object@sad == "ls") {
    LL <- function(N, alpha) -sum(drad(object@sad, object@data$x, log=TRUE, list(N=N, alpha=alpha), NaN))
    result <- do.call("mle2", list(LL, eval.only=TRUE, start=list(N=object@fullcoef[1], alpha=object@fullcoef[2])))
  } else stop ("this distribution has not been implemented yet")

  return(result)
}

#fitmand <- function(x, trunc, start.value, ...){
#  if(class(x)!="rad") rad.tab <- rad(x)
#  else rad.tab <- x
#  N <- max(rad.tab$rank)
#  y <- rep(rad.tab$rank, rad.tab$abund)
#  dots <- list(...)
#  if (!missing(trunc)){
#    if (min(y)<=trunc) stop("truncation point should be lower than the lowest data value")
#  }
#  if(missing(start.value)){
#	x75 <- rad.tab[(floor(dim(rad.tab)[1]/4)):dim(rad.tab)[1],]
#    x75 <- x75[x75$abund > 1, ]
#	shat <- - coef(lm(log(abund)~log(rank), data=x75))[2]
#    vhat <- 30
#  }
#  else{
#    shat <- start.value[1]
#    vhat <- start.value[2]
#  }
#  if (missing(trunc)){
#    LL <- function(N, s, v) -sum(dmand(y, N = N, s=s, v=v, log = TRUE))
#  }
#  else{
#    LL <- function(N, s, v) -sum(dtrunc("mand", x = y, coef = list(N = N, s = s, v = v), trunc = trunc, log = TRUE))
#  }
#  result <- do.call("mle2", c(list(LL, start = list(s = shat, v = vhat), fixed=list(N=N), data = list(x = y)), dots))
#  new("fitrad", result, rad="mand", distr = distr.depr, trunc = ifelse(missing(trunc), NaN, trunc), rad.tab=rad.tab)
#}
