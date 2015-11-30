qls<-function(p, N, alpha, lower.tail = TRUE, log.p = FALSE){
  if(any(p != sort(p))) stop("p vector should be in ascending order")
  if(length(N) > 1 | length(alpha) > 1) stop("Vectorization not implemented for the parameters")
	if (log.p) p <- exp(p)
	if(!lower.tail) p <- 1 - p
  y <- rep(0, length(p))
  y[1] <- suppressWarnings(qfinder(dls, p[i], list(N=N, alpha=alpha), 0))
  for (i in 2:length(p))
    y[i] <- suppressWarnings(qfinder(dls, p[i], list(N=N, alpha=alpha), y[i-1]))
#	q <- function(p) suppressWarnings(qfinder(dls, p, list(N=N, alpha=alpha)))
#	y <- sapply(p, q)
	if(any(is.nan(y))) warning("NaNs produced")
	return(y)
}
