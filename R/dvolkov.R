dvolkov <- function(x, theta, m, J, N=2000, log=FALSE){
	if(length(theta) > 1 | length(m) > 1 | length(J) > 1) stop ("Vectorization of parameters not implemented")
	if(!is.finite(theta) | theta <= 0 | !is.finite(J) | J <= 0 | m <= 0 | m >= 1) return (rep(NaN, length(x)))
	Cvolkov <- .C(volkov, res=as.double(rep(0,J)), theta0=as.double(theta), m0=as.double(m), J0=as.integer(J), N0=as.integer(N))
	y <- Cvolkov$res[x]
	y[ !is.wholenumber(x) ] <- NaN
	if (any(is.nan(y))) warning ("NaNs produced")
	if(log) return(log(y))
	else return(y)
}

dvolkov.original <- function(x, theta, m, J, log=FALSE){
	if(length(theta) > 1 | length(m) > 1 | length(J) > 1) stop ("Vectorization of parameters not implemented")
	if(!is.finite(theta) | theta <= 0 | !is.finite(J) | J <= 0 | m <= 0 | m >= 1) return (rep(NaN, length(x)))
	gam <- m*(J-1)/(1-m)
	# some preliminary calculations to speed up
	# lJm is the term that only depends on J and m
	lJm <- lgamma(J+1) + lgamma(gam) - lgamma(J+gam)
	integrand <- function(y, n, ln){
		theta * exp(lJm + ln + 
					lgamma(n+y) + lgamma(J-n+gam-y) - lgamma(1+y) - lgamma(gam-y)-
					y*theta/gam
					)
	}   
	f <- function(n){
		# ln is the term that only depends on J, m, and n
		ln <- - lgamma(n+1) - lgamma(J-n+1)
		integrate(integrand, lower=0, upper=gam, n=n, ln=ln)$value
	}   
	vals <- sapply(1:J,f)
	Stot <- sum(vals, na.rm=T)
	y <- vals[x]/Stot
	if (any(is.nan(y))) warning ("NaNs produced")
	if(log) return(log(y))
	else return(y)
}
