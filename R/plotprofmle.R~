plotprofmle <- function(mleobj, nseg=7, ratio=log(8), which=NULL )
{
	if( class(mleobj)[1] != "profile.mle" &
           class(mleobj)[1] != "profile.mle2") 
		stop( "Object should have class \'profile.mle\' or \'profile.mle2\'")
	mleprof <- mleobj@profile
	npar <- length(mleprof)
	if( is.null(which) )
		parseq = 1:npar
	else
		parseq = which
	for(i in parseq)
	{
		tmp <- mleprof[i][[1]]
		varname <- names(mleprof[i])
		y <- tmp[,1]^2/2
		x <- (tmp[,2][,i])
		interpol = spline(x, y, n=nseg*length(x) )
		plot(interpol, 
			type="l", 
			xlab=varname, 
			ylab="Negative Log-Likelihood",
			col="red",
		)
		x.verint = interpol$x[ interpol$y <= ratio ]
		y.verint = rep( ratio, length(x.verint) ) 
		lines(x.verint, y.verint , col="blue", lty=2)
		lines(rep(x.verint[1],2), c(-1,ratio), col="blue", lty=2)
		lines(rep(x.verint[length(x.verint)],2), c(-1,ratio), col="blue", lty=2)
	}
}
