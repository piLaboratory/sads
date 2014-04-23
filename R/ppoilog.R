ppoilog <- function(q, mu, sig, lower.tail=TRUE, log.p=FALSE){
  if (length(mu) > 1 | length(sig) > 1) stop("vectorization of mu and sig is currently not implemented")
  if (!all(is.finite(c(mu, sig)))) stop("all parameters should be finite")
  if (sig <= 0) stop("sig must be larger than zero")
  z <- cumsum(poilog::dpoilog(0:max(q), mu, sig))
  y <- z[q+1]
  if(!lower.tail) y <- 1-y
  if(log.p) y <- log(y)
  return(y)
}