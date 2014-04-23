qqsad <- function(object, sad, coef, trunc=NA, distr, plot=TRUE){
  if(class(object)=="fitsad"){
    sad <- object@sad
    coef <- as.list(bbmle::coef(object))
    trunc <- object@trunc
    distr <- object@distr
    x <- object@data$x
  }
  else if(class(object)=="numeric")
    x <- object
  rank <- sort(x)
  S <- length(x)
  if(distr == "D"){
    q <- 1:sum(x)
    if(!is.na(trunc)){
      if(sad == "ls")
        p <- do.call(ptrunc, list(sad, q = q, coef = c(list(N=(sum(x)),coef)), trunc = trunc))
      else if(sad == "volkov"|| sad=="mzsm")
        p <- do.call(ptrunc, list(sad, q = q, coef = c(list(J=(sum(x)),coef)), trunc = trunc))
      else
        p <- do.call(ptrunc, list(sad, q = q, coef=coef, trunc=trunc))
    }
    else{
      psad <- get(paste("p", sad, sep=""), mode = "function")
      if(sad == "ls")
        p <- do.call(psad, c(list(q = q, N = sum(x)), coef))
      else if(sad =="volkov"||sad=="mzsm")
        p <- do.call(psad, c(list(q = q, J=sum(x)), coef))
      else{
        p <- do.call(psad, c(list(q = q), coef))
      }
    }
    f1 <- approxfun(x=c(1, p), y=c(0, q), method="constant")
    q <- f1(ppoints(S))
  }
  else if(distr == "C"){
    p <- ppoints(S)
    if(!is.na(trunc))
      q <- do.call(qtrunc, list(sad, p = p, trunc = trunc, coef=coef))
    else{
      qsad <- get(paste("q", sad, sep=""), mode = "function")
      q <- do.call(qsad, c(list(p = p), coef))
    }
  }
  else
    stop("please choose 'D'iscrete or 'Continuous' for 'distr'")
  if(plot){
    plot(q, rank, main = "Q-Q plot", xlab="Theoretical Quantile", ylab="Sample Quantiles")
    abline(0, 1, col = "red", lty = 2)
  }
  return(invisible(data.frame(theoret.q=q, sample.q=rank)))
}
