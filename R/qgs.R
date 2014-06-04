qgs<-function(p, k, S, lower.tail = TRUE, log.p = FALSE){
  if (length(S) > 1) stop("vectorization of S is not implemented")
  if (S <= 0)  stop("S must positive integer")
  if(k<0||k>1) stop("k must be larger than zero and no more than one")
  if (log.p) p <- exp(p)
  if(!lower.tail) p <- 1 - p
  ## Ugly: just to make qgs(1, ...) = S
  p[p==1] <- 1+1e-12
  Y <- 1:S
  X <- pgs(Y, k=k, S=S)
  f1 <- approxfun(x=X, y=Y, method="constant", rule=2)
  f1(p)              
}
