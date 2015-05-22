# trueLL generic and methods
setGeneric("trueLL", 
		   def=function(object, dist, coef, trunc, dec.places = 0, breaks, counts, log = TRUE, ...)
		   standardGeneric("trueLL")
		   )
setMethod("trueLL", signature(object="fitsad", dist="missing", coef="missing", trunc="missing", 
							  dec.places="ANY", breaks="ANY", counts="ANY", log="ANY"),
		  function(object, dist, coef, trunc, dec.places = 0, breaks, counts, log = TRUE, ...){
			  trueLL(object=object@data$x, dist=object@sad, coef=as.list(bbmle::coef(object)),
					 trunc=object@trunc, dec.places=dec.places, breaks=breaks, counts=counts, log=log, ...)
		  })
# Default invocation, object is numeric
setMethod("trueLL", signature(object="numeric", dist="character", coef="list", trunc="ANY", 
							  dec.places="ANY", breaks="ANY", counts="ANY", log="ANY"),
		  function(object, dist, coef, trunc, dec.places = 0, breaks, counts, log = TRUE, ...){
			  dots <- list(...)
			  if(missing(breaks)){
				  D <- 10^(-dec.places)/2
				  object <- round(object, dec.places)
				  if(missing(trunc) | is.nan(trunc)){
					  cdf <- get(paste("p", dist, sep=""), mode = "function")
					  probs <- do.call(cdf, c(list(q = object+D), as.list(coef), dots)) - do.call(cdf, c(list(q = object-D), as.list(coef), dots))
				  }
				  else{
					  probs <- do.call(ptrunc, c(list(dist, q = object+D, coef = as.list(coef), trunc=trunc), dots))-
					  do.call(ptrunc, c(list(dist, q = object-D, coef = as.list(coef), trunc=trunc), dots))
				  }
			  }
			  else{
				  h1 <- hist(object, breaks=breaks, plot=FALSE)
				  if(missing(trunc) | is.nan(trunc)){
					  cdf <- get(paste("p", dist, sep=""), mode = "function")
					  probs <- diff(do.call(cdf, c(list(q = h1$breaks), as.list(coef), dots)))
				  }
				  else{
					  probs <- diff(do.call(ptrunc, c(list(dist, q = h1$breaks, coef = as.list(coef), trunc=trunc), dots)))
				  }
				  if(missing(counts)) counts <- h1$counts
				  probs <- rep(probs, counts)
			  }
			  if(log) sum(log(probs)) else prod(probs)
		  })
