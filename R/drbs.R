drbs <- function(x, N, S, log = FALSE) {
  if (any(x < 1)) warning("q=at least one x less than one")
  if (S < 1) stop("S must be positive integer")
  if (N < 1) stop("N must be positive integer")
  if (!any(is.wholenumber(x))) warning("all x must be integers")
  if(any(x > S)) stop("at least one q larger than S")
  y <- sapply(x, function(z)N/S*sum((z:S)^-1))/N
  if(!log) return(y)
  else return(log(y))
}
