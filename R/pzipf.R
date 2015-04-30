pzipf <- function(q, N, s, lower.tail = TRUE, log.p = FALSE){
	q[ ! is.wholenumber(q) | q < 1] <- NaN
	N[ !is.finite(N) | N <= 0 | !is.wholenumber(N) ] <- NaN
	s[ !is.finite(s) | s <= 0] <- NaN
	y <- c()
	for (i in 1:length(q)){
		if (q[i] == 0) y[i] <- NaN
		else y[i] <- log(sum(1/(1:q[i])^s)) - log(sum(1/(1:N)^s))
	}
	y <- exp(y)
	if(!lower.tail) y <- 1-y
	if(log.p) y <- log(y)
	if (any(is.nan(y))) warning ("NaNs produced")
	return(y)
}
