dpoilog <- function(x, mu, sig, log = FALSE){
	x[ ! is.wholenumber(x) | x < 0] <- NaN
	mu[ !is.finite(mu) ] <- NaN
	sig[ !is.finite(sig) | sig <= 0] <- NaN
	y <- poilog::dpoilog(x, mu, sig)
	if (any(is.nan(y))) warning ("NaNs produced")
	if (log) return(log(y))
	else return(y)
}
