dpoig <- function(x, frac, rate, shape, log=FALSE) {
	if(any(!is.wholenumber(x)))
		stop("dpoig is a discrete PDF; all x's must be integers")
	b <- x*log(frac)+shape*log(rate)+lgamma(x+shape)
	c <- lfactorial(x)+lgamma(shape)+(x+shape)*log(frac+rate)
	vals<-exp(b-c)
	if(log)log(vals) else vals
}
