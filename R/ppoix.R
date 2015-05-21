ppoix <- function(q, frac, rate, lower.tail=TRUE, log.p=FALSE) {
	y <- suppressWarnings(cumsumW("poix", q, list(frac=frac, rate=rate), lower.tail, log.p, pad=FALSE))
	if(any(is.nan(y))) warning("NaNs produced")
	return(y)
}
