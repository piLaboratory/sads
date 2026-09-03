fitparetoC <- function(x, trunc, trunc.max, start.value, upper = 20, ...){
    dots <- list(...)
    if (any(x$breaks < 0)) stop ("All x must be positive")
    if (!missing(trunc.max)){
        if (max(x$breaks)>trunc.max) stop("trunc.max should not be lower than the highest data value")
    }
    if(missing(start.value)){
        y <- rep(x$mids, x$counts)
        alpha <- length(y)/sum(log(y)-log(min(y)))
        beta  <-  min(x$mids)
    }
    else{
        alpha <- start.value[1]
        beta  <- start.value[2]
    }
    trunc.args <- list()
    if (!missing(trunc)) trunc.args$trunc <- trunc
    if (!missing(trunc.max)) trunc.args$trunc.max <- trunc.max
    if (length(trunc.args) > 0){
        LL <- function(shape, scale) -do.call(trueLL, c(list(x, dist = "pareto",
                                             coef = list(shape = shape, scale = scale)), trunc.args))
    }
    else {
        LL <- function(shape, scale) -trueLL(x, dist = "pareto",
                                             coef = list(shape = shape, scale = scale))
    }
    ## result <- do.call("mle2", c(list(LL, start = list(shape = alpha, scale= beta),
    ##                                  method = "L-BFGS-B",
    ##                                  upper= c(shape = upper, scale=min(x$breaks)+0.001)), dots))
    result <- do.call("mle2", c(list(LL, start = list(shape = alpha, scale= beta)), dots))
    if(abs(as.numeric(result@coef[1]) - upper) < 0.001)
        warning("mle equal to upper bound provided. \n Try value for the 'upper' argument")
    new("fitsadC", result, sad="pareto", trunc = ifelse(missing(trunc), NaN, trunc), trunc.max = ifelse(missing(trunc.max), NaN, trunc.max), hist = x)
}
