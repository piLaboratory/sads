pmand <- function (q, N, s, v, lower.tail = TRUE, log.p = FALSE){
  if (v <= 0 || s <= 0){
    warning("Function not defined for v or s <= zero, NaN's returned")
    y <- rep(NaN, length(q))
  }
  else{
    if (N < 1||!any(is.wholenumber(N))) 
      stop("N must be positive integer")
    y <- NULL
    for (i in 1:length(q)) {
      y[i] <- log(sum(1/((1:q[i])+v)^s)) - log(sum(1/((1:N)+v)^s))
    }
    y <- exp(y)
    if (!lower.tail) 
      y <- 1 - y
    if (log.p) 
      y <- log(y)
  }
  return(y)
}
