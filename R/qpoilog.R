qpoilog<-function(p, mu, sig, lower.tail = TRUE, log.p = FALSE){
	if(!lower.tail) p <- 1 - p
  op <- rank(p, ties.method="max")
  p <- sort(p)
  if(length(mu) > 1 | length(sig) > 1) stop("Vectorization not implemented for the parameters")
	if (log.p) p <- exp(p)
  y <- c()
  y[1] <- suppressWarnings(qfinder(dpoilog, p[1], list(mu=mu, sig=sig), 0))
  if(length(p) > 1)
    for (i in 2:length(p))
      y[i] <- suppressWarnings(qfinder(dpoilog, p[i], list(mu=mu, sig=sig), y[i-1]))
	if(any(is.nan(y))) warning("NaNs produced")
	return(y[op])
}
