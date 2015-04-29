is.wholenumber	<-function(x, tol = .Machine$double.eps^0.5){
  abs(x - round(x)) < tol
}

# Wrapper for cumsum, implementing some
# error checking, converting NA to NaN for 
# coherence with [dpqr] functions and
# padding with 0 for 1-based distributions
# such as gs
cumsumW <- function(f, q, coef, lower.tail, log.p, pad) {
	if (any(sapply(coef, length) > 1)) stop("Vectorization not implemented for the parameters")
	dist <- get(paste("d", f, sep=""), mode="function")
	if (pad)
		z <- c(NaN, cumsum(do.call(dist, c(list(x=1:max(q)), coef))))
	else
		z <- cumsum(do.call(dist, c(list(x=0:max(q)), coef)))
	y <- z[q+1]
	y[! is.wholenumber(q) ] <- NaN
  if(!lower.tail) y <- 1-y
  if(log.p) y <- log(y)
	y[is.na(y)] <- NaN
	return (y)
}
