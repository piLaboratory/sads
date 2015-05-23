dpowbend <- function(x, s, omega, log = FALSE){
	if(omega == 0) return(dpower(x, s, log))
	s[ !is.finite(s) | s <= 1 ] <- NaN
	omega[ !is.finite(omega) | omega < 0 ] <- NaN
	y <- x^(-s) * exp(-omega * x) / LiE(s, -omega)
	if (any(is.nan(y))) warning ("NaNs produced")
	if (any(!is.wholenumber(x))) warning("non integer values in x")
	y[ ! is.wholenumber(x) | x < 1] <- 0
	if(log) return(log(y))
	else return(y)
}
