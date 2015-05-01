## Uses code from untb::volkov, by Robin K. S. Hankin
## http://cran.r-project.org/web/packages/untb/
dvolkov <- function(x, theta, m, J, log=FALSE){
	theta[ !is.finite(theta) | theta <= 0] <- NaN
	J[ !is.finite(J) | J <= 0] <- NaN
	m[ m < 0 | m > 1 ] <- NaN
	vals <- volkov(J,theta,m)
	Stot <- sum(vals, na.rm=T)
	y <- vals[x]/Stot
	if (any(is.nan(y))) warning ("NaNs produced")
	if(log) return(log(y))
	else return(y)
}

volkov <- function(J, theta, m){
  gam <- m*(J-1)/(1-m)
  integrand <- function(y,n){
    theta*
      exp(lgamma(J+1) - lgamma(n+1) - lgamma(J-n+1) + 
          lgamma(gam) - lgamma(J+gam) + 
          lgamma(n+y) + lgamma(J-n+gam-y) - lgamma(1+y) - lgamma(gam-y)-
          y*theta/gam
          )
  }
  
  f <- function(n){
	  integrate(integrand, lower=0, upper=gam, n=n)$value
  }
  out <- sapply(1:J,f)
  return(out)
}

