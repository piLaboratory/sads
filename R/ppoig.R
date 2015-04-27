ppoig <- function(q, frac, rate, shape, lower.tail=TRUE, log.p=FALSE) {
  if (length(frac) > 1 | length(rate) > 1 | length(shape) > 1) 
	  stop("vectorization of parameters is currently not implemented")
  z <- cumsum(dpoig(0:max(q), frac, rate, shape))
  y <- z[q+1]
  if(!lower.tail) y <- 1-y
  if(log.p) y <- log(y)
  return(y)
}
