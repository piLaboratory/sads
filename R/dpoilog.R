dpoilog <- function(x, mu, sig, log = FALSE){
  if (!all(is.finite(c(mu, sig)))) stop("all parameters should be finite")
  if (sig <= 0) stop("sig must be larger than zero")
  if (any(x < 0)) stop("at least one x less than zero")
  if (any(!is.wholenumber(x))) stop("all x must be integers")
  y <- poilog::dpoilog(x, mu, sig)
  if (log) return(log(y))
  else return(y)
}