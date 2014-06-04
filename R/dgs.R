dgs <- function(x, k, S,  log = FALSE) {
  if (any(x < 1)) warning("at least one x less than one")
  if (S < 1) stop("S must be positive integer")
  if(k<0||k>1) stop("k must be larger than zero and no more than one")
  if(any(x > S)) stop("at least one x larger than S")
  if (!any(is.wholenumber(x))) warning("all x must be integers")
  cf <- 1/(1-(1-k)^S)
  y <- cf*k*(1-k)^(x-1)
  if(!log) return(y)
  else return(log(y))
}
