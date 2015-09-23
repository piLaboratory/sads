ppowbend <- function(q, s, omega, lower.tail=TRUE, log.p=FALSE){
	if (any(!is.wholenumber(q))) warning("non integer values in q")
	y <- suppressWarnings(cumsumW("powbend", q, list(s=s, omega=omega), lower.tail, log.p, pad=FALSE))
	if(any(is.nan(y))) warning("NaNs produced")
	return(y)
}
