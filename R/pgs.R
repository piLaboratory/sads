pgs <- function(q, k, S, lower.tail=TRUE, log.p = FALSE) {
	y <- suppressWarnings(cumsumW("gs", q, list(k=k, S=S), lower.tail, log.p, pad=TRUE))
	if(any(is.nan(y))) warning("NaNs produced")
	return(y)
}
