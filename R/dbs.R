dbs <- function(x, N, S, log = FALSE){
	N[ !is.finite(N) | N <= 0] <- NaN
	S[ !is.finite(S) | S <= 0] <- NaN
	y <- (S-1)*(1-x/N)^(S-2)/N
	if (any(is.nan(y))) warning ("NaNs produced")
	if (any(!is.wholenumber(x))) warning("non integer values in x")
	y[ ! is.wholenumber(x) | x < 1 | x > S] <- 0
	if(log) return (log(y))
	else return(y)
}
