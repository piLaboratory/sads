dzipf <- function(x, N, s, log = FALSE) {
	x[ ! is.wholenumber(x) | x < 1] <- NaN
	N[ !is.finite(N) | N <= 0 | !is.wholenumber(N) ] <- NaN
	s[ !is.finite(s) | s <= 0] <- NaN
	y <- -s*log(x)-log(sum(1/(1:N)^s))
	if (any(is.nan(y))) warning ("NaNs produced")
	if(log) return(y)
	else return(exp(y))
}
