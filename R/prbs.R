prbs <- function(q, N, S, lower.tail=TRUE, log.p = FALSE){
  if(length(N) > 1 | length(S) > 1) stop("Vectorization not implemented for the parameters")
  nonwhole <- !is.wholenumber(q)
	y <- suppressWarnings(cumsumW(drbs, q, list(N=N, S=S), lower.tail, log.p, pad=TRUE))
	if (any(nonwhole)) {
    warning("non integer values in q")
    if(log.p) y[nonwhole] <- -Inf
    else y[nonwhole] <- 0
  }
	if(any(is.nan(y))) warning("NaNs produced")
	return(y)
}
