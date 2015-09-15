dmzsm <- function(x, J, theta, log = FALSE){
	J[ !is.finite(J) | J <= 0] <- NaN
	theta[ !is.finite(theta) | theta <= 0] <- NaN
	mzsm <- function(y, J, theta) (theta/y)*(1-y/J)^(theta-1)
	sn <- mzsm(y=x, J = J, theta = theta)
	mu <- mzsm(y=1:J, J = J, theta = theta)
	lpn <- log(sn) - log(sum(mu))
        lpn[ x <= 0 | x > J ] <- -Inf
	if (any(is.nan(lpn))) warning ("NaNs produced")
	if(log) return(lpn)
	else return(exp(lpn))
}
