pgs <- function(q, k, N, S, lower.tail=TRUE, log.p = FALSE){
  if (!all(is.finite(c(N, S)))) stop("all parameters should be finite")
  if (N < 1)  stop("N must be positive integer")
  if (S < 1)  stop("S must be positive integer")
  if(any(q < 1)) stop("at least one q less than one")
  if(any(q > S)) stop("at least one q larger than S")
  if(any(!is.wholenumber(q))) stop("all q must be integers")
  if(k<0||k>1) stop("k must be larger than zero and no more than one")
  z <- cumsum(dgs(1:max(q), k, N, S))
  y <- z[q]
  if(!lower.tail) y <- 1-y
  if(log.p) y <- log(y)
  return(y)
}
