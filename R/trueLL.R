trueLL <- function(x, dens, coef, trunc, dec.places = 0, log = TRUE, ...){
  dots <- list(...)
  D <- 10^(-dec.places)/2
  x <- round(x, dec.places)
  if(missing(trunc)){
    cdf <- get(paste("p", dens, sep=""), mode = "function")
    probs <- do.call(cdf, c(list(q = x+D), as.list(coef), dots)) - do.call(cdf, c(list(q = x-D), as.list(coef), dots))
  }
  else{
    probs <- do.call(ptrunc, c(list(dens, q = x+D, coef = as.list(coef), trunc=trunc), dots))-
    do.call(ptrunc, c(list(dens, q = x-D, coef = as.list(coef), trunc=trunc), dots))
  }
  if(log) sum(log(probs)) else prod(probs)
}
