pls <- function(q, N, alpha, lower.tail=TRUE, log.p=FALSE){
  if (length(N) > 1 | length(alpha) > 1) stop("vectorization of N and alpha is currently not implemented")
  if (!all(is.finite(c(N, alpha)))) stop("all parameters should be finite")
  if (N <= 0)  stop("N must be larger than zero")
  if (alpha <= 0) stop("alpha must be larger than zero")
  z <- cumsum(dls(1:max(q), N, alpha))
  y <- z[q]
  if(!lower.tail) y <- 1-y
  if(log.p) y <- log(y)
  return(y)
}