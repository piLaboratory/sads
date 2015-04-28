qpoilog<-function(p, mu, sig, lower.tail = TRUE, log.p = FALSE){
  if (length(mu) > 1 | length(sig) > 1) stop("vectorization of mu and sig is currently not implemented")
  if (!all(is.finite(c(mu, sig)))) stop("all parameters should be finite")
  if (sig <= 0) stop("sig must be larger than zero")
  if (log.p) p <- exp(p)
  if(!lower.tail) p <- 1 - p
  q <- function(p) qfinder("poilog", p, list(mu=mu, sig=sig))
  return(sapply(p, q))
}
