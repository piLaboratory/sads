dvolkov <- function(x, theta, m, J, log=FALSE){
	theta[ !is.finite(theta) | theta <= 0] <- NaN
	J[ !is.finite(J) | J <= 0] <- NaN
	m[ m < 0 | m > 1 ] <- NaN
	if (any(!is.wholenumber(x))) warning("non integer values in x")
	x[ ! is.wholenumber(x) | x < 0] <- 0
	vals <- c(0, volkov(J,c(theta,m))) # index 0 should receive 0
	Stot <- sum(vals, na.rm=T)
	y <- vals[x+1]/Stot
	if (any(is.nan(y))) warning ("NaNs produced")
	if (any(!is.wholenumber(x))) warning("non integer values in x")
	if(log) return(log(y))
	else return(y)
}
