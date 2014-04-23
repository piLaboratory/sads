dmzsm <- function(x, J, theta, log = FALSE){
  #if (J <= 0) stop ("J must be great than zero")
  #if (theta <= 0) stop ("theta must be great than zero")
  if (theta <= 0 || J <= 0){
    warning("Function not defined for theta or J <= zero, NaN's returned")
    lpn <- rep(NaN, length(x))
  }
  else{
    mzsm <- function(y, J, theta) (theta/y)*(1-y/J)^(theta-1)
    sn <- mzsm(y=x, J = J, theta = theta)
    mu <- mzsm(y=1:J, J = J, theta = theta)
    lpn <- log(sn) - log(sum(mu))
  }
  if(log) return(lpn)
  else return(exp(lpn))
}
