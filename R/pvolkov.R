pvolkov <- function(q, theta, m , J, lower.tail=TRUE, log.p=FALSE){
  if(length(theta) > 1 | length(m) > 1 | length(J) > 1) stop("Vectorization not implemented for the parameters")
  nonwhole <- !is.wholenumber(q)
	y <- suppressWarnings(cumsumW(dvolkov, q, list(theta=theta, m=m, J=J), lower.tail, log.p, pad=TRUE))
	if (any(nonwhole)) {
    warning("non integer values in q")
    if(log.p) y[nonwhole] <- -Inf
    else y[nonwhole] <- 0
  }
	if(any(is.nan(y))) warning("NaNs produced")
	return(y)
}
