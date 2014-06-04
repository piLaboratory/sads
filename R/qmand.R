qmand <- function (p, N, s, v, lower.tail = TRUE, log.p = FALSE) {
  if (N < 1||!any(is.wholenumber(N))) 
    stop("N must be positive integer")
  d <- NULL
  if (log.p) 
    p <- exp(p)
  if(!lower.tail) p <- 1 - p
  ## Ugly: just to make qgs(1, ...) = S
  p[p==1] <- 1+1e-12
  Y <- 1:N
  X <- pmand(Y, N=N, s=s, v=v)
  f1 <- approxfun(x=X, y=Y, method="constant", rule=2)
  f1(p)
}
