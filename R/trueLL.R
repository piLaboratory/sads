# trueLL generic and methods
setGeneric("trueLL", 
		   def=function(object, dist, coef, trunc, trunc.max, dec.places=0, breaks, ...)
		   standardGeneric("trueLL")
		   )
setMethod("trueLL", signature(object="fitsad", dist="missing", coef="missing", trunc="missing", trunc.max="missing",
							  dec.places="ANY", breaks="ANY"),
		  function(object, dec.places=0, breaks, ...){
			  if(missing(breaks))
					  return(trueLL(object=object@data$x, dist=object@sad, coef=as.list(bbmle::coef(object)),
							 trunc=object@trunc, trunc.max=object@trunc.max, dec.places=dec.places, ...))
			  else 
					  return(trueLL(object=object@data$x, dist=object@sad, coef=as.list(bbmle::coef(object)),
							 trunc=object@trunc, trunc.max=object@trunc.max, breaks=breaks, ...))
		  })
setMethod("trueLL", signature(object="fitrad", dist="missing", coef="missing", trunc="missing", trunc.max="missing",
							  dec.places="ANY", breaks="ANY"),
		  function(object, dec.places=0, breaks, ...){
			  if(missing(breaks))
				  return(trueLL(object=object@rad.tab$abund, dist=object@rad, coef=as.list(bbmle::coef(object)),
						 trunc=object@trunc, trunc.max=object@trunc.max, dec.places=dec.places, breaks=breaks, ...))
			  else
				  return(trueLL(object=object@rad.tab$abund, dist=object@rad, coef=as.list(bbmle::coef(object)),
						 trunc=object@trunc, trunc.max=object@trunc.max, breaks=breaks, ...))
		  })
# object is numeric
setMethod("trueLL", signature(object="numeric", dist="character", coef="list", trunc="ANY", trunc.max="ANY",
							  dec.places="ANY", breaks="ANY"),
		  function(object, dist, coef, trunc, trunc.max, dec.places=0, breaks, ...){
			  dots <- list(...)
			  if(missing(breaks)){
				  D <- 10^(-dec.places)/2
				  breaks <- sort(unique(c(object - D, object + D)))
			  }
			  h1 <- hist(object, breaks=breaks, plot=FALSE)
			  if(missing(trunc)) trunc <- NaN
			  if(missing(trunc.max)) trunc.max <- NaN
			  trueLL(object=h1, dist=dist, coef=coef, trunc=trunc, trunc.max=trunc.max)
		  })
# workhorse method, all other methods redirect here
setMethod("trueLL", signature(object="histogram", dist="character", coef="list", trunc="ANY", trunc.max="ANY",
							  dec.places="missing", breaks="missing"),
		  function(object, dist, coef, trunc, trunc.max, ...){
			  dots <- list(...)
			  if(missing(trunc)) trunc <- NaN
			  if(missing(trunc.max)) trunc.max <- NaN
              if( any (object$breaks < 0) || (! is.nan(trunc) && any(object$breaks < trunc)))
                  stop("Invalid values: some x are being counted below 0 or the truncation point")
              if( ! is.nan(trunc.max) && any(object$breaks > trunc.max))
                  stop("Invalid values: some x are being counted above trunc.max")
			  if(is.nan(trunc) && is.nan(trunc.max)){
				  cdf <- get(paste("p", dist, sep=""), mode = "function")
				  probs <- diff(do.call(cdf, c(list(q = object$breaks), as.list(coef), dots)))
			  }
			  else{
                  trunc.args <- list()
                  if(!is.nan(trunc)) trunc.args$trunc <- trunc
                  if(!is.nan(trunc.max)) trunc.args$trunc.max <- trunc.max
				  probs <- diff(do.call(ptrunc, c(list(dist, q = object$breaks, coef = as.list(coef)), trunc.args, dots)))
			  }
			  probs <- rep(probs, object$counts)
			  return(sum(log(probs)))
		  })
