dmand <- function (x, N, s, v, log = FALSE) {
  if (any(x < 1)) 
    warning("the Zipf-Mandelbrot's distribution is not set to zero or negative numbers")
  if (N < 1||!any(is.wholenumber(N)))
    stop("N must be positive integer")
  if (!any(is.wholenumber(x))) 
    warning("x must be integer")
  if (s <= 0 || v < 0){
    warning("Function not defined for s <=zero or v < zero, NaN's returned")
    lny <- rep(NaN, length(x))
  }
  else
    lny <- - s * log(x+v) - log(sum(((1:N)+v)^(-s)))
  if (log) return(lny)
  else return(exp(lny))
}
