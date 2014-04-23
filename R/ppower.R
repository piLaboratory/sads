ppower <- function(q, s, lower.tail = TRUE, log.p = FALSE){
  if (s <= 1) stop("s must be greater than one")
  y <- NULL
  for (i in 1:length(q)){
    y[i] <- log(sum(1/(1:q[i])^s)) - log(zeta(s))
  }
  y <- exp(y)
  if(!lower.tail) y <- 1 - y
  if(log.p) y <- log(y)
  return(y)
}