dls <- function(x, N, alpha, log = FALSE){
	x[ ! is.wholenumber(x) | x < 1] <- NaN
	N[ !is.finite(N) | N <= 0] <- NaN
	alpha[ !is.finite(alpha) | alpha <= 0] <- NaN
	X <- N/(N+alpha)
	gama <- function(y) {1/log(1/(1-y))}
	y <- gama(X)*(X^x)/x
	if (any(is.nan(y))) warning ("NaNs produced")
	if(log) return (log(y))
	else return(y)
}
