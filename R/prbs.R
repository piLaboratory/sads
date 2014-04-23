prbs <- function(q, N, S, lower.tail=TRUE, log.p = FALSE){
  if (!all(is.finite(c(N, S)))) stop("all parameters should be finite")
  if (N <= 0)  stop("N must be positive integer")
  if (S <= 0)  stop("S must be positive integer")
  if(any(q < 1)) stop("at least one q less than one")
  if(any(!is.wholenumber(q))) stop("all q must be integers")
  if(any(q > S)) stop("at least one q larger than S")
  z <- cumsum(drbs(1:max(q), N, S))
  y <- z[q]
  if(!lower.tail) y <- 1-y
  if(log.p) y <- log(y)
  return(y)
}
