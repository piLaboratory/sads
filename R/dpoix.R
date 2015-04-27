dpoix <- function(x, frac, rate, log=FALSE) {
	if(any(!is.wholenumber(x)))
		stop("dpoix is a discrete PDF; all x's must be integers")
	b <- x*log(frac)
	m <- log(rate)
	n <- (x+1)*log(rate+frac)
	vals <- exp(b+m-n)
	if(log) log(vals) else vals
}


