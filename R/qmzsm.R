qmzsm<-function(p, J, theta, lower.tail = TRUE, log.p = FALSE){
  if(length(J) > 1 | length(theta) > 1) stop("Vectorization not implemented for the parameters")
  if (!all(is.finite(c(J, theta))) | theta <= 0 | J <= 0)
	  return(rep(NaN, length(p)))
	if (log.p) p <- exp(p)
	if(!lower.tail) p <- 1 - p
  Y <- 1:J
  X <- pmzsm(Y, theta=theta, J=J)
  approx(X,Y,xout=p,method="constant",f=0,yleft=1, yright=J)$y
}
