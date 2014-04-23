dvolkov <- function(x, theta, m, J, log=FALSE){
  if (theta <= 0 || J <= 0 || m < 0 || m > 1){
    warning("Function not defined for theta or J <= zero, m <0 or m > 1 \n NaN's returned")
    vals <- rep(NaN, length(x))
  }
  else{
    vals <- volkov(J,c(theta,m))
    Stot <- sum(vals, na.rm=T)
  }
  if(log)log(vals[x]/Stot)
  else vals[x]/Stot
}
