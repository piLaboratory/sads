setGeneric("plotprofmle", 
    def=function(object, nseg=20, ratio=log(8), which=NULL, ask=NULL, col.line="blue", varname=NULL, ...) standardGeneric("plotprofmle")
    )
setMethod("plotprofmle", "profile.mle2",
function(object, nseg=20, ratio=log(8), which=NULL, ask=NULL, col.line="blue", varname=NULL, ...){
  mleprof <- object@profile
  npar <- length(mleprof)
  if(missing(which))
    which <- 1:npar
  if(missing(ask))
    ask <- (prod(par("mfcol")) < length(which)) && dev.interactive()
  dots <- list(...)
  if(!"ylab" %in% names(dots)) dots$ylab <- "Negative relative log-likelihood"
  if(!"type" %in% names(dots)) dots$type <- "l"
  if(!"col" %in% names(dots)) dots$col <- "red"
  vname <- names(mleprof)
  if( is.null(which) ){
    if(!missing(varname)){
      if(length(varname)!=length(mleprof))stop("Length of 'varname' should match number os mles in mle.prof object")
      vname <- varname
    }
    parseq = 1:npar
  }
  else{
    if(!missing(varname)){
      if(length(varname)!=length(which))stop("Length of 'which' should match length of 'varnames'")
      vname[which] <- varname
    }
    parseq = which
  }
  if(ask){
    oask <- devAskNewPage(TRUE)
    on.exit(devAskNewPage(oask))
  }
  for(i in parseq) {
    prof <- internal.spline(mleprof, i, ratio, nseg)
    do.call(plot, c(list(x=prof$x, y=prof$y, xlab=vname[i]),dots))
    L <- length(prof$lower) # Is the same as length(prof$upper)
    if(L > 0)
      for (j in 1:L) {
        xx <- prof$x[1]/2 + prof$x[2]/2 + 1e-12
        lines(c(prof$lower[j],prof$upper[j]),c(ratio, ratio), col=col.line, lty=2)
        if(prof$lower[j] > xx) # dont draw vertical lines at the borders
          lines(rep(prof$lower[j],2), c(-1, ratio), col=col.line, lty=2)
        if(prof$upper[j] < max(prof$x)) # dont draw vertical lines at the borders
          lines(rep(prof$upper[j],2), c(-1, ratio), col=col.line, lty=2)
      }
  }
})
setMethod("plotprofmle", "mle2",
          function(object, nseg=20, ratio=log(8), which=NULL, ask=NULL, col.line="blue", varname=NULL, ...){
    warning("Running a profile on the object. You should consider storing the profile in a different variable.")
    plotprofmle(profile(object), nseg, ratio, which, ask, col.line, varname, ...)
})

internal.spline <- function(mleprof, i, ratio, nseg) {
      tmp <- mleprof[i][[1]]
      y <- tmp[,1]^2/2
      x <- (tmp[,2][,i])
      interpolF = splinefun(x, y, method="monoH.FC")
      # Redo the x axis to increase the number of points in nseg times
      l = nseg*length(x); a <- (max(x) - min(x))/(l-1)
      x <- a * 1:l + min(x)-a
      y <- interpolF(x)
      lower <- c()
      upper <- c()
      if(!is.null(ratio)){
        l <- length(y)
        # Finds where the interpolation crosses the "y = ratio" line
        change <- (y - ratio)[2:l] * (y - ratio)[1:(l-1)]
        endpoints <- which(change < 0)
        # Adds the borders, if any of them is lower than ratio
        if(y[1] < ratio) endpoints <- c(1, endpoints)
        if(y[l] < ratio) endpoints <- c(endpoints, l)
        corr <- (x[2]-x[1])/2
        for (j in 1:(length(endpoints)/2)) {
          lower <-c(lower, x[endpoints[(2*j)-1]]+corr)
          upper <-c(upper, x[endpoints[2*j]]+corr)
        }
      }
      return(list(x=x,y=y,lower=lower, upper=upper))
}

setGeneric("likelregions", 
    def=function(object, nseg=100, ratio=log(8), ...) standardGeneric("likelregions")
    )
setMethod("likelregions", "profile.mle2",
function(object, nseg=100, ratio=log(8), ...){
  mleprof <- object@profile
  npar <- length(mleprof)
  which <- 1:npar
  out <- list()
  for(i in which) {
    prof <- internal.spline(mleprof, i, ratio, nseg)
    xx <- prof$x[2]/2 + prof$x[2]/2 + 1e-12 # "minimum" x

    int <- list()
    L <- length(prof$lower) # Is the same as length(prof$upper)
    if(L > 0)
      for (j in 1:L) {
        int[[j]] <- c(prof$lower[j], prof$upper[j])
        if(int[[j]][1] < xx) int[[j]][1] <- NA
        if(int[[j]][2] > max(prof$x)) int[[j]][2] <- NA
      }
    out[[i]] <- int
  }
  new("likelregions", out, names=names(mleprof), ratio=ratio)
})
setMethod("likelregions", "mle2",
function(object, nseg=100, ratio=log(8), ...){
    warning("Running a profile on the object. You should consider storing the profile in a different variable.")
    likelregions(profile(object), nseg, ratio, ...)
})

