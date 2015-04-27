ppoix <- function(q, frac, rate, lower.tail=TRUE, log.p=FALSE) {
  if (length(frac) > 1 | length(rate) > 1) stop("vectorization of frac and rate is currently not implemented")
  z <- cumsum(dpoix(0:max(q), frac, rate))
  y <- z[q+1]
  if(!lower.tail) y <- 1-y
  if(log.p) y <- log(y)
  return(y)
}
