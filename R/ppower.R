ppower <- function(q, s, lower.tail = TRUE, log.p = FALSE){
	s[ !is.finite(s) | s <= 1 ] <- NaN
	y <- c()
	for (i in 1:length(q)){
		if (is.nan(q[i])) y[i] <- NaN
		else y[i] <- log(sum(1/(1:q[i])^s)) - log(zeta(s))
	}
	y <- exp(y)
	if (any(!is.wholenumber(q))) warning("non integer values in q")
	y[ ! is.wholenumber(q) | q < 1 ] <- 0
	if(!lower.tail) y <- 1 - y
	if(log.p) y <- log(y)
	if (any(is.nan(y))) warning ("NaNs produced")
	return(y)
}
