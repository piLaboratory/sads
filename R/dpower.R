dpower <- function(x, s, log = FALSE){
	s[ !is.finite(s) | s <= 1 ] <- NaN
	y <- -s*log(x)-log(zeta(s))
	if (any(is.nan(y))) warning ("NaNs produced")
	if (any(!is.wholenumber(x))) warning("non integer values in x")
	y[ ! is.wholenumber(x) | x < 1] <- 0
	if(log) return(y)
	else return(exp(y))
}
