dzipf <- function(x, N, s, log = FALSE) {
  if (any(x < 1)) warning("the zipf's distribution is not set to zero or negative numbers")
  if (s <= 0) stop("s must be greater than zero")
  if (N < 1) stop("N must be positive integer")
  if (!any(is.wholenumber(x))) warning("x must be integer")
  y <- NULL
  for (i in 1:length(x)){
    if(!is.wholenumber(x[i])) y[i] <- -Inf
    else y[i] <- -s*log(x[i])-log(sum(1/(1:N)^s))
  }
  if(log) return(y)
  else return(exp(y))
}
