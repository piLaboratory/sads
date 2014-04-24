pvolkov <- function(q, theta, m , J, lower.tail=TRUE, log.p=FALSE){
  if(length(J) > 1 | length(theta) > 1) stop("vectorization of J and theta is currently not implemented")
  if(length(m)>1) stop("vectorization of m is currently not implemented")
  if(!all(is.finite(c(J, theta, m)))) stop("all parameters should be finite")
  if (theta <= 0 || J <= 0 || m < 0 || m > 1){
    warning("Function not defined for theta or J <= zero, m <0 or m > 1 \n NaN's returned")
    y <- rep(NaN, length(q))
  }
  else{
    z <- cumsum(dvolkov(x = 1:max(q), theta = theta, J = J, m = m))
    y <- z[q]
  }
  if(!lower.tail) y <- 1-y
  if(log.p) y <- log(y)
  return(y)
}
