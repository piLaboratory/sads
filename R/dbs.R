dbs <- function(x, N, S, log = FALSE){
  if (!all(is.finite(c(N, S)))) stop("all parameters should be finite")
  if (N <= 0)  stop("N must be larger than zero")
  if (S <= 0)  stop("S must be larger than zero")
  if(any(x < 1)) stop("at least one x less than one")
  if(any(!is.wholenumber(x))) stop("all x must be integers")
  y <- (S-1)*(1-x/N)^(S-2)/N
  if(log) return (log(y))
  else return(y)
}
