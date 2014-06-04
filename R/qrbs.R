qrbs<-function(p, N, S, lower.tail = TRUE, log.p = FALSE){
  if (length(N) > 1) stop("vectorization of N is not implemented")
  if (length(S) > 1) stop("vectorization of S is not implemented")
  if (!all(is.finite(c(N, S)))) stop("all parameters should be finite")
  if (N <= 0)  stop("N must positive integer")
  if (S <= 0)  stop("S must positive integer")
  d<-NULL
  if (log.p) p <- exp(p)
  if(!lower.tail) p <- 1-p
  ## Ugly: just to make qgs(1, ...) = S
  p[p==1] <- 1+1e-12
  Y <- 1:S
  X <- prbs(Y, N=N, S=S)
  f1 <- approxfun(x=X, y=Y, method="constant", rule=2)
  f1(p)
}
