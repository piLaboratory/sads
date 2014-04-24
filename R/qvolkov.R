qvolkov<-function(p, theta, m, J, lower.tail = TRUE, log.p = FALSE){
  if (length(theta) > 1 | length(m) > 1 |length(J) > 1) stop("vectorization of theta, m and J is not implemented")
  if (!all(is.finite(c(J, theta, m)))) stop("all parameters should be finite")
  if (theta <= 0 || J <= 0 || m < 0 || m > 1){
    warning("Function not defined for theta or J <= zero, m <0 or m > 1 \n NaN's returned")
    vals <- rep(NaN, length(p))
  }
  else{
    if (log.p) p <- exp(p)
    if(!lower.tail) p <- 1 - p
    Y <- 1:J
    X <- pvolkov(Y, theta=theta, m=m, J=J)
    f1 <- approxfun(X,Y,method="constant",f=0,yleft=1, yright=J)
    vals <- f1(p)
  }
  vals
}
