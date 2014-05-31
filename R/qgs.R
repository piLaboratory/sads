qgs<-function(p, k, S, lower.tail = TRUE, log.p = FALSE){
  if (length(S) > 1) stop("vectorization of S is not implemented")
  #if (!all(is.finite(c(N, S)))) stop("all parameters should be finite")
  if (S <= 0)  stop("S must positive integer")
  if(k<0||k>1) stop("k must be larger than zero and no more than one")
  d<-NULL
  if (log.p) p <- exp(p)
  if(!lower.tail) rule=2 else rule=1
  Y <- 1:S
  X <- pgs(Y, k=k, S=S, lower.tail=lower.tail)
  f1 <- approxfun(x=X, y=Y, method="constant", rule=rule)
  f1(p)
}
