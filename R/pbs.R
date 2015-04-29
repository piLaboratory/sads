pbs <- function(q, N, S, lower.tail=TRUE, log.p = FALSE){
	y <- suppressWarnings(cumsumW("bs", q, list(N=N, S=S), lower.tail, log.p, pad=TRUE))
	if(any(is.nan(y))) warning("NaNs produced")
	return(y)
}
