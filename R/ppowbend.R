ppowbend <- function(q, s, omega = 0.01, oM = -log(omega), lower.tail=TRUE, log.p=FALSE){
  if(length(s) > 1 | length(oM) > 1) stop("Vectorization not implemented for the parameters")
  if (!missing(omega) && !missing(oM)) 
    stop("specify 'omega' or 'oM' but not both")
  nonwhole <- !is.wholenumber(q)
	y <- suppressWarnings(cumsumW(dpowbend, q, list(s=s,oM=oM), lower.tail, log.p, pad=FALSE))
	if (any(nonwhole)) {
    warning("non integer values in q")
    if(log.p) y[nonwhole] <- -Inf
    else y[nonwhole] <- 0
  }
	if(any(is.nan(y))) warning("NaNs produced")
	return(y)
}
