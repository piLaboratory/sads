dvolkov <- function(x, theta, m, J, log=FALSE){
	if(length(theta) > 1 | length(m) > 1 | length(J) > 1) stop ("Vectorization of parameters not implemented")
	if(!is.finite(theta) | theta <= 0 | !is.finite(J) | J <= 0 | m <= 0 | m >= 1) return (rep(NaN, length(x)))

	Cvolkov <- .C(volkov, res=as.double(rep(0,J)), theta0=as.double(theta), m0=as.double(m), J0=as.integer(J))
	y <- Cvolkov$res[x]
	y[ !is.wholenumber(x) ] <- NaN
	if (any(is.nan(y))) warning ("NaNs produced")
	if(log) return(log(y))
	else return(y)
}
