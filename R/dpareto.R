dpareto <- function(x, shape, scale = min(x), log = FALSE){
  if(any(x < scale))
    stop("scale parameter must be equal or greater than min(x)")
  if(shape <= 0 || scale <= 0){
    warning("Function not defined for shape or scale <= zero, NaN's returned")
    lny <- rep(NaN, length(x))
  }
  else
    lny <- log(shape) + shape*log(scale) - (shape+1)*log(x)
  if (log) return(lny)
  else return(exp(lny))
}
