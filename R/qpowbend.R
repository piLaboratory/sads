qpowbend<-function(p, s, omega = 0.1, oM = -log(omega), lower.tail = TRUE, log.p = FALSE){
  if (!missing(omega) && !missing(oM)) 
    stop("specify 'omega' or 'oM' but not both")
	if (log.p) p <- exp(p)
	if(!lower.tail) p <- 1 - p
	q <- function(p) suppressWarnings(qfinder("powbend", p, list(s=s, oM=oM)))
	y <- sapply(p, q)
	if(any(is.nan(y))) warning("NaNs produced")
	return(y)
}
