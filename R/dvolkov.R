## Uses code from untb::volkov, by Robin K. S. Hankin
## http://cran.r-project.org/web/packages/untb/
dvolkov <- function(x, theta, m, J, log=FALSE){
	if(length(theta) > 1 | length(m) > 1 | length(J) > 1) stop ("Vectorization of parameters not implemented")
	if(!is.finite(theta) | theta <= 0 | !is.finite(J) | J <= 0 | m <= 0 | m >= 1) return (rep(NaN, length(x)))

	vals <- .C(volkov, x0=as.double(x), theta0=as.double(theta), m0=as.double(m), J0=as.integer(J))
	print(vals)
	return();
	gam <- m*(J-1)/(1-m)
	# some preliminary calculations to speed up
	# lJm is the term that only depends on J and m
	lJm <- lgamma(J+1) + lgamma(gam) - lgamma(J+gam)
	integrand <- function(y, n, ln){
		exp(ln + lgamma(n+y) + lgamma(J-n+gam-y) - lgamma(1+y) - lgamma(gam-y)- y*theta/gam)
	}
	f <- function(n){
		# ln is the term that only depends on J, m, and n
		ln <- lJm - lgamma(n+1) - lgamma(J-n+1)
		theta * integrate(integrand, lower=0, upper=gam, n=n, ln=ln)$value
	}
	vals <- sapply(1:J,f)
	Stot <- sum(vals, na.rm=T)
	y <- vals[x]/Stot
	if (any(is.nan(y))) warning ("NaNs produced")
	if(log) return(log(y))
	else return(y)
}
