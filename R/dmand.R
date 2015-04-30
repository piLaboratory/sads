dmand <- function (x, N, s, v, log = FALSE) {
	x[ ! is.wholenumber(x) | x < 1] <- NaN
	N[ !is.finite(N) | N < 1 | !is.wholenumber(N) ] <- NaN
	s[ !is.finite(s) | s <= 0] <- NaN
	v[ !is.finite(v) | v < 0] <- NaN
	lny <- - s * log(x+v) - log(sum(((1:N)+v)^(-s)))
	if (any(is.nan(lny))) warning ("NaNs produced")
	if (log) return(lny)
	else return(exp(lny))
}
