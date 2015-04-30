dpareto <- function(x, shape, scale = min(x), log = FALSE){
	x [ x < scale ] <- NaN
	shape[ !is.finite(shape) | shape <= 0] <- NaN
	scale[ !is.finite(scale) | scale <= 0] <- NaN
    lny <- log(shape) + shape*log(scale) - (shape+1)*log(x)
	if (any(is.nan(lny))) warning ("NaNs produced")
  if (log) return(lny)
  else return(exp(lny))
}
