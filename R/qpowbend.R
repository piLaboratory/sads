qpowbend<-function(p, s, omega = 0.01, oM = -log(omega), lower.tail = TRUE, log.p = FALSE){
	if(!lower.tail) p <- 1 - p
  op <- rank(p, ties.method="max")
  p <- sort(p)
  if(length(s) > 1 | length(oM) > 1) stop("Vectorization not implemented for the parameters")
  if (!missing(omega) && !missing(oM)) 
    stop("specify 'omega' or 'oM' but not both")
	if (log.p) p <- exp(p)
  y <- c()
  y[1] <- suppressWarnings(qfinder(dpowbend, p[1], list(s=s, oM=oM), 0))
  if(length(p) > 1)
    for (i in 2:length(p))
      y[i] <- suppressWarnings(qfinder(dpowbend, p[i], list(s=s, oM=oM), y[i-1]))
	if(any(is.nan(y))) warning("NaNs produced")
	return(y[op])
}
