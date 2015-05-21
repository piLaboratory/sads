ppoig <- function(q, frac, rate, shape, lower.tail=TRUE, log.p=FALSE) {
	y <- suppressWarnings(cumsumW("poig", q, list(frac=frac, rate=rate, shape=shape), lower.tail, log.p, pad=FALSE))
	if(any(is.nan(y))) warning("NaNs produced")
	return(y)
}
