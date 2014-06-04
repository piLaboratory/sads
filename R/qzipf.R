qzipf <- function(p, N, s, lower.tail = TRUE, log.p = FALSE){
  if (s <= 0)
    stop("s must be greater than zero")
  if (N < 1||!any(is.wholenumber(N)))
    stop("N must be positive integer")
  if(log.p) p <- exp(p)
  if(!lower.tail) p <- 1-p
  ## Ugly: just to make qgs(1, ...) = N
  p[p==1] <- 1+1e-12
  Y <- 1:N
  X <- pzipf(Y, N=N, s=s)
  f1 <- approxfun(x=X, y=Y, method="constant", rule=2)
  f1(p)
}
