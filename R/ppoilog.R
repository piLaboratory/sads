ppoilog <- function(q, mu, sig, lower.tail=TRUE, log.p=FALSE){
  if(length(mu) > 1 | length(sig) > 1) stop("Vectorization not implemented for the parameters")
  nonwhole <- !is.wholenumber(q)
	y <- suppressWarnings(cumsumW(dpoilog, q, list(mu=mu, sig=sig), lower.tail, log.p, pad=FALSE))
	if (any(nonwhole)) {
    warning("non integer values in q")
    if(log.p) y[nonwhole] <- -Inf
    else y[nonwhole] <- 0
  }
	if(any(is.nan(y))) warning("NaNs produced")
	return(y)
}
