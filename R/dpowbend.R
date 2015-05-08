dpowbend <- function(x, s, omega, log = FALSE){
	# Circumventing errors in function zeta
	to.NaN <- !is.finite(s) | s <= 1
	s[to.NaN] <- 0
	y <- x^(-s) * exp(-omega * x) / LiE(s, -omega)
	y[to.NaN] <- NaN
	if (any(is.nan(y))) warning ("NaNs produced")
	if (any(!is.wholenumber(x))) warning("non integer values in x")
	y[ ! is.wholenumber(x) | x < 1] <- 0
	if(log) return(log(y))
	else return(y)
}
