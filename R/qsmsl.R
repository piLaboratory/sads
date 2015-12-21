qsmsl<-function(p, delta, epsilon, lower.tail = TRUE, log.p = FALSE){
  if (length(delta) > 1 | length(epsilon) > 1) stop("vectorization of parameters is not implemented")
	delta[ !is.finite(delta) | delta <= 0] <- NaN
	epsilon[ !is.finite(epsilon) | epsilon <= 0 | epsilon > 1/delta ] <- NaN
  if(log.p) p <- exp(p)
  if(!lower.tail) p <- 1-p
  Y <- 1:(1/delta)
  X <- psmsl(Y, delta=delta, epsilon=epsilon)
  approx(x=X, y=Y, xout=p, method="linear", rule=2, ties="ordered")$y
}
