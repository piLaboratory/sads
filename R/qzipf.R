qzipf <- function(p, N, s, lower.tail = TRUE, log.p = FALSE){
  if (s <= 0)
    stop("s must be greater than zero")
  if (N < 1||!any(is.wholenumber(N)))
    stop("N must be positive integer")
  if(log.p) p <- exp(p)
  if(!lower.tail)
    rule=2 else rule=1
  Y <- 1:N
  X <- pzipf(Y, N=N, s=s, lower.tail=lower.tail)
  f1 <- approxfun(x=X, y=Y, method="constant", rule=rule)
  f1(p)
}
