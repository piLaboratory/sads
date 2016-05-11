qls<-function(p, N, alpha, lower.tail = TRUE, log.p = FALSE){
	if(!lower.tail) p <- 1 - p
  op <- rank(p, ties.method="max")
  p <- sort(p)
  if(length(N) > 1 | length(alpha) > 1) stop("Vectorization not implemented for the parameters")
	if (log.p) p <- exp(p)
  y <- c()
  y[1] <- suppressWarnings(qfinder(dls, p[1], list(N=N, alpha=alpha), 0))
  if(length(p) > 1)
    for (i in 2:length(p))
      y[i] <- suppressWarnings(qfinder(dls, p[i], list(N=N, alpha=alpha), y[i-1]))
	if(any(is.nan(y))) warning("NaNs produced")
	return(y[op])
}
