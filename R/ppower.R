ppower <- function(q, s, lower.tail = TRUE, log.p = FALSE){
	q[ ! is.wholenumber(q) | q < 1 ] <- NaN
	s[ !is.finite(s) | s <= 1 ] <- NaN
	y <- c()
	for (i in 1:length(q)){
		y[i] <- log(sum(1/(1:q[i])^s)) - log(zeta(s))
	}
	y <- exp(y)
	if(!lower.tail) y <- 1 - y
	if(log.p) y <- log(y)
	if (any(is.nan(y))) warning ("NaNs produced")
	return(y)
}
