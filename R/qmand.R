qmand <- function (p, N, s, v, lower.tail = TRUE, log.p = FALSE) {
  if (N < 1||!any(is.wholenumber(N))) 
    stop("N must be positive integer")
  d <- NULL
  if (log.p) 
    p <- exp(p)
  if (!lower.tail) 
    rule=2 else rule=1
  Y <- 1:N
  X <- pmand(Y, N=N, s=s, v=v, lower.tail=lower.tail)
  f1 <- approxfun(x=X, y=Y, method="constant", rule=rule)
  f1(p)
}
