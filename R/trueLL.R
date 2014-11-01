trueLL <- function(x, dens, coef, trunc, dec.places = 0, breaks, counts, log = TRUE, ...){
    dots <- list(...)
    if(missing(breaks)){
        D <- 10^(-dec.places)/2
        x <- round(x, dec.places)
        if(missing(trunc)){
            cdf <- get(paste("p", dens, sep=""), mode = "function")
            probs <- do.call(cdf, c(list(q = x+D), as.list(coef), dots)) - do.call(cdf, c(list(q = x-D), as.list(coef), dots))
        }
        else{
            probs <- do.call(ptrunc, c(list(dens, q = x+D, coef = as.list(coef), trunc=trunc), dots))-
                do.call(ptrunc, c(list(dens, q = x-D, coef = as.list(coef), trunc=trunc), dots))
        }
    }
    else{
        h1 <- hist(x, breaks=breaks, plot=FALSE)
        if(missing(trunc)){
            cdf <- get(paste("p", dens, sep=""), mode = "function")
            probs <- diff(do.call(cdf, c(list(q = h1$breaks), as.list(coef), dots)))
        }
        else{
            probs <- diff(do.call(ptrunc, c(list(dens, q = h1$breaks, coef = as.list(coef), trunc=trunc), dots)))
        }
        if(missing(counts)) counts <- h1$counts
        probs <- rep(probs, counts)
    }
    if(log) sum(log(probs)) else prod(probs)
}
