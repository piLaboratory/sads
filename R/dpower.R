dpower <- function(x, s, log = FALSE){
	x[ ! is.wholenumber(x) | x < 1 ] <- NaN
	s[ !is.finite(s) | s <= 1 ] <- NaN
	y <- -s*log(x)-log(zeta(s))
	if (any(is.nan(y))) warning ("NaNs produced")
	if(log) return(y)
	else return(exp(y))
}
