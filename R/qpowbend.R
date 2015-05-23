qpowbend<-function(p, s, omega, lower.tail = TRUE, log.p = FALSE){
	if (log.p) p <- exp(p)
	if(!lower.tail) p <- 1 - p
	q <- function(p) suppressWarnings(qfinder("powbend", p, list(s=s, omega=omega)))
	y <- sapply(p, q)
	if(any(is.nan(y))) warning("NaNs produced")
	return(y)
}
