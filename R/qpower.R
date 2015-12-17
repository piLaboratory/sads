qpower <- function(p, s, lower.tail = TRUE, log.p = FALSE){
	if(!lower.tail) p <- 1 - p
  op <- rank(p, ties.method="max")
  p <- sort(p)
  if(length(s) > 1) stop("Vectorization not implemented for the parameters")
	if (log.p) p <- exp(p)
  y <- c()
  y[1] <- suppressWarnings(qfinder(dpower, p[1], list(s=s), 0))
  if(length(p) > 1)
    for (i in 2:length(p))
      y[i] <- suppressWarnings(qfinder(dpower, p[i], list(s=s), y[i-1]))
	if(any(is.nan(y))) warning("NaNs produced")
	return(y[op])
}
