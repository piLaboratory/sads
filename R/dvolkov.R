dvolkov <- function(x, theta, m, J, log=FALSE){
	theta[ !is.finite(theta) | theta <= 0] <- NaN
	J[ !is.finite(J) | J <= 0] <- NaN
	m[ m < 0 | m > 1 ] <- NaN
	vals <- volkov(J,c(theta,m))
	Stot <- sum(vals, na.rm=T)
	y <- vals[x]/Stot
	if (any(is.nan(y))) warning ("NaNs produced")
	if(log) return(log(y))
	else return(y)
}
