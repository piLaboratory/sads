pmzsm <- function(q, J, theta, lower.tail=TRUE, log.p=FALSE){
  if(length(J) > 1 | length(theta) > 1) stop("Vectorization not implemented for the parameters")
  nonwhole <- !is.wholenumber(q)
	y <- suppressWarnings(cumsumW(dmzsm, q, list(J=J, theta=theta), lower.tail, log.p, pad=TRUE))
	if (any(nonwhole)) {
    warning("non integer values in q")
    if(log.p) y[nonwhole] <- -Inf
    else y[nonwhole] <- 0
  }
	if(any(is.nan(y))) warning("NaNs produced")
	return(y)
}
