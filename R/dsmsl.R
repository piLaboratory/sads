dsmsl <- function(x, delta, eps, log = FALSE){
  x[x <= 0] <- NaN
	delta[ !is.finite(delta) | delta <= 0] <- NaN
	eps[ !is.finite(eps) | eps <= 0] <- NaN
  c1 <- 1/(delta*eps - log(delta*eps) - 1)
  y <- c1 * (1/x - delta)
  y[ x < eps | x > 1/delta] <- 0
	if (any(is.nan(y))) warning ("NaNs produced")
	if(log) return(log(y))
	else return(y)
}
