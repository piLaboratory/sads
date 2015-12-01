ppowbend <- function(q, s, omega = 0.01, oM = -log(omega), lower.tail=TRUE, log.p=FALSE){
  if (!missing(omega) && !missing(oM)) 
    stop("specify 'omega' or 'oM' but not both")
	if (any(!is.wholenumber(q))) warning("non integer values in q")
	y <- suppressWarnings(cumsumW("powbend", q, list(s=s, oM=oM), lower.tail, log.p, pad=FALSE))
	if(any(is.nan(y))) warning("NaNs produced")
	return(y)
}
