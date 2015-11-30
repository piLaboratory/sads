qmzsm<-function(p, J, theta, lower.tail = TRUE, log.p = FALSE){
  if(any(p != sort(p))) stop("p vector should be in ascending order")
  if(length(J) > 1 | length(theta) > 1) stop("Vectorization not implemented for the parameters")
	if (log.p) p <- exp(p)
	if(!lower.tail) p <- 1 - p
  y <- c()
  y[1] <- suppressWarnings(qfinder(dmzsm, p[1], list(J=J, theta=theta), 0))
  if(length(p) > 1)
    for (i in 2:length(p))
      y[i] <- suppressWarnings(qfinder(dmzsm, p[i], list(J=J, theta=theta), y[i-1]))
	if(any(is.nan(y))) warning("NaNs produced")
	return(y)
}
