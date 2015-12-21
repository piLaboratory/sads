dsmsl <- function(x, delta, epsilon, log = FALSE){
	delta[ !is.finite(delta) | delta <= 0] <- NaN
	epsilon[ !is.finite(epsilon) | epsilon <= 0] <- NaN
  c1 <- 1/(delta*epsilon - log(delta*epsilon) - 1)
  y <- c1 * (1/x - delta)
  y[ x < epsilon | x > 1/delta] <- 0
	if (any(is.nan(y))) warning ("NaNs produced")
	if(log) return(log(y))
	else return(y)
}
