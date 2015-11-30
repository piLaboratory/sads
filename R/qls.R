qls<-function(p, N, alpha, lower.tail = TRUE, log.p = FALSE){
  if(length(N) > 1 | length(alpha) > 1) stop("Vectorization not implemented for the parameters")
	if (log.p) p <- exp(p)
	if(!lower.tail) p <- 1 - p
	q <- function(p) suppressWarnings(qfinder("ls", p, list(N=N, alpha=alpha)))
	y <- sapply(p, q)
	if(any(is.nan(y))) warning("NaNs produced")
	return(y)
}
