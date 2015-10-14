setGeneric("plotprofmle", 
    def=function(object, nseg=20, ratio=log(8), which=NULL, ask=NULL, col.line="blue", varname=NULL, ...) standardGeneric("plotprofmle")
    )
setMethod("plotprofmle", "profile.mle2",
function(object, nseg, ratio, which, ask, col.line, varname, ...){
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
  for(i in parseq)
    {
      prof <- internal.spline(mleprof, i, ratio)

      do.call(plot, c(list(x=prof$x, y=prof$y, xlab=vname[i]),dots))
		if(length(prof$endpoints) > 0)
        for (j in 1:(length(prof$endpoints)/2)) {
          lower <-x[prof$endpoints[(2*j)-1]]+corr
          upper <-x[prof$endpoints[2*j]]+corr
          lines(c(lower,upper ),c(ratio, ratio), col=col.line, lty=2)
          if(prof$endpoints[(2*j-1)] != 1) # dont draw vertical lines at the borders
            lines(rep(lower,2), c(-1, ratio), col=col.line, lty=2)
          if(prof$endpoints[(2*j)] != l) # dont draw vertical lines at the borders
            lines(rep(upper,2), c(-1, ratio), col=col.line, lty=2)
        }
    }
})
setMethod("plotprofmle", "mle2",
    function(object, ...) {
    cat("NOTICE: Running a profile on the object. You should consider storing the profile\n")
    cat("in a different variable\n")
    plotprofmle(profile(object), ...)
})

internal.spline <- function(mleprof, i, ratio) {
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
        # Finds where the ation crosses the "y = ratio" line
        change <- (y - ratio)[2:l] * (y - ratio)[1:(l-1)]
        endpoints <- which(change < 0)
        # Adds the borders, if any of them is lower than ratio
        if(y[1] < ratio) endpoints <- c(1, endpoints)
        if(y[l] < ratio) endpoints <- c(endpoints, l)
        corr <- (x[2]-x[1])/2
      }
      return(list(x=x,y=y,endpoints=endpoints))
}

