ppsad <- function (object, sad, coef, trunc=NA, plot=TRUE) {
  if(class(object)=="fitsad"){
    sad <- object@sad
    coef <- as.list(bbmle::coef(object))
    trunc <- object@trunc
    x <- object@data$x
  }
  else if(class(object)=="numeric")
    x <- object
  rank <- sort(x)
  S <- length(x)
  z <- ppoints(S)
  if(!is.na(trunc)){
    if(sad == "ls")
      p <- do.call(ptrunc, list(sad, q = rank, coef = c(list(N=(sum(x)),coef)), trunc = trunc))
    else if(sad == "volkov"||sad=="mzsm")
      p <- do.call(ptrunc, list(sad, q = rank, coef = c(list(J=(sum(x)),coef)), trunc = trunc))
    else
      p <- do.call(ptrunc, list(sad, q = rank, coef = coef, trunc = trunc))
  }
  else{
    psad <- get(paste("p", sad, sep=""), mode = "function")
    if(sad == "ls")
      p <- do.call(psad, c(list(q = rank, N=sum(x)), coef))
    else if(sad == "volkov"||sad=="mzsm")
      p <- do.call(psad, c(list(q = rank, J=sum(x)), coef))
    else{
      p <- do.call(psad, c(list(q = rank), coef))
    }
  }
  if(plot){
    plot(p, z, main = "P-P plot", ylim = c(0, 1), xlab='Theoretical Percentiles', ylab='Sample Percentiles')
    abline(0, 1, col = "red", lty = 2)
  }
  return(invisible(data.frame(theoret.p=p, sample.p=z)))
}
