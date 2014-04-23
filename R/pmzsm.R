pmzsm <- function(q, J, theta, lower.tail=TRUE, log.p=FALSE){
  if (length(J) > 1 || length(theta) > 1) stop("vectorization of J and theta is currently not implemented")
  if (!all(is.finite(c(J, theta)))) stop("all parameters should be finite")
  # dzsm has this warning
  #if (theta <= 0 || J <= 0){
  #  warning("Function not defined for theta or J <= zero, NaN's returned")
  #  y <- rep(NaN, length(x))
  #}
  else{
    z <- cumsum(dmzsm(x = 1:max(q), J = J, theta = theta))
    y <- z[q]
  }
  if(!lower.tail) y <- 1-y
  if(log.p) y <- log(y)
  return(y)
}
