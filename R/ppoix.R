ppoix <- function(q, frac, rate, lower.tail=TRUE, log.p=FALSE) {
  if(length(frac) > 1 | length(rate) > 1) stop("Vectorization not implemented for the parameters")
  nonwhole <- !is.wholenumber(q)
	y <- suppressWarnings(cumsumW(dpoix, q, list(frac=frac, rate=rate), lower.tail, log.p, pad=FALSE))
	if (any(nonwhole)) {
    warning("non integer values in q")
    if(log.p) y[nonwhole] <- -Inf
    else y[nonwhole] <- 0
  }
	if(any(is.nan(y))) warning("NaNs produced")
	return(y)
}
