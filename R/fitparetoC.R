fitparetoC <- function(x, trunc, start.value, upper = 20, ...){
    dots <- list(...)
    if (any(x$breaks < 0)) stop ("All x must be positive")
    if(missing(start.value)){
        y <- rep(x$mids, x$counts)
        alpha <- length(y)/sum(log(y)-log(min(y))) 
        beta  <-  min(x$mids)
    }
    else{
        alpha <- start.value[1]
        beta  <- start.value[2]
    }
    if (missing(trunc)){
        LL <- function(shape, scale) -trueLL(x, dist = "pareto",
                                             coef = list(shape = shape, scale = scale))
    }
    else {
        LL <- function(shape, scale) -trueLL(x, dist = "pareto",
                                             coef = list(shape = shape, scale = scale), trunc = trunc)
    }  
    ## result <- do.call("mle2", c(list(LL, start = list(shape = alpha, scale= beta),
    ##                                  method = "L-BFGS-B",
    ##                                  upper= c(shape = upper, scale=min(x$breaks)+0.001)), dots))
    result <- do.call("mle2", c(list(LL, start = list(shape = alpha, scale= beta)), dots))
    if(abs(as.numeric(result@coef[1]) - upper) < 0.001) 
        warning("mle equal to upper bound provided. \n Try value for the 'upper' argument")
    new("fitsadC", result, sad="pareto", trunc = ifelse(missing(trunc), NaN, trunc), hist = x) 
}
