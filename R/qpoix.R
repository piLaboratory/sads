qpoix <- function(p, frac, rate, lower.tail=TRUE, log.p=FALSE) {
	if (log.p) p <- exp(p)
	if(!lower.tail) p <- 1 - p
	q <- function(p) suppressWarnings(qfinder("poix", p, list(frac=frac, rate=rate)))
	y <- sapply(p, q)
	if(any(is.nan(y))) warning("NaNs produced")
	return(y)
}
