qrbs<-function(p, N, S, lower.tail = TRUE, log.p = FALSE){
  if (length(N) > 1) stop("vectorization of N is not implemented")
  if (length(S) > 1) stop("vectorization of S is not implemented")
  if (!all(is.finite(c(N, S)))) stop("all parameters should be finite")
  if (N <= 0)  stop("N must positive integer")
  if (S <= 0)  stop("S must positive integer")
  d<-NULL
  if (log.p) p <- exp(p)
  if(!lower.tail) rule=2 else rule=1
  Y <- 1:S
  X <- prbs(Y, N=N, S=S, lower.tail=lower.tail)
  f1 <- approxfun(x=X, y=Y, method="constant", rule=rule)
  f1(p)
}
