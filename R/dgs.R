dgs <- function(x, k, S,  log = FALSE) {
	x[ ! is.wholenumber(x) | x < 1 | x > S ] <- NaN
	S[ !is.finite(S) | S < 1 | !is.wholenumber(S) ] <- NaN
	k[ k<0 | k>1 ] <- NaN
	cf <- 1/(1-(1-k)^S)
	y <- cf*k*(1-k)^(x-1)
	if (any(is.nan(y))) warning ("NaNs produced")
	if(!log) return(y)
	else return(log(y))
}
