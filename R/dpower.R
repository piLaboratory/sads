dpower <- function(x, s, log = FALSE){
  if (any(x < 1)) warning("the zipf's distribution is not set to x <= zero")
  if (!any(is.wholenumber(x))) warning("x must be integer")
  y <- NULL
  if (s <= 1) {
    warning("Function not defined for s <= zero, NaN's returned")
    y <- rep(NaN, length(x))
  }
  else{
    for (i in 1:length(x)){
      if(!is.wholenumber(x[i])) y[i] <- -Inf
      else y[i] <- -s*log(x[i])-log(zeta(s))
    }
  }
  if(log) return(y)
  else return(exp(y))
}
