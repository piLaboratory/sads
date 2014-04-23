pprad <- function (object, rad, coef, trunc=NA, plot=TRUE) {
  if(class(object)=="fitrad"){
    rad <- object@rad
    coef <- as.list(bbmle::coef(object))
    trunc <- object@trunc
    x <- sort(object@data$x)
  }
  else if(class(object)=="numeric")
    x <- rep(1:length(object),sort(object,decreasing=T))
  N <- length(x)
  z <- ppoints(N)
  if(!is.na(trunc)){
    p <- do.call(ptrunc, list(rad, q = x, coef = coef, trunc = trunc))
  }
  else{
    prad <- get(paste("p", rad, sep=""), mode = "function")
    p <- do.call(prad, c(list(q = x), coef))
  }
  if(plot){
    plot(z, p, main = "P-P plot", ylim = c(0, 1), xlab='Theoretical Percentiles', ylab='Sample Percentiles')
    abline(0, 1, col = "red", lty = 2)
  }
  return(invisible(data.frame(theoret.p=p, sample.p=z)))
}
