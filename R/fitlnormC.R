fitlnormC <- function(x, trunc, trunc.max, start.value, ...){
    dots <- list(...)
    if (any(x$breaks < 0)) stop ("All x must be positive")
    if (!missing(trunc.max)){
        if (max(x$breaks)>trunc.max) stop("trunc.max should not be lower than the highest data value")
    }
    if(missing(start.value)){
        y <- rep(x$mids, x$counts)
        meanlog <- mean(log(y))
        sdlog <- sd(log(y))
    }
    else{
        meanlog <- start.value[1]
        sdlog <-start.value[2]
    }
    trunc.args <- list()
    if (!missing(trunc)) trunc.args$trunc <- trunc
    if (!missing(trunc.max)) trunc.args$trunc.max <- trunc.max
    if (length(trunc.args) > 0){
        LL <- function(meanlog, sdlog) -do.call(trueLL, c(list(x, dist = "lnorm", coef = list(meanlog = meanlog, sdlog = sdlog)), trunc.args))
    }
    else {
        LL <- function(meanlog, sdlog) -trueLL(x, dist = "lnorm", coef = list(meanlog = meanlog, sdlog = sdlog))
    }
    result <- do.call("mle2", c(list(LL, start = list(meanlog = meanlog, sdlog = sdlog)), dots))
    new("fitsadC", result, sad="lnorm", trunc = ifelse(missing(trunc), NaN, trunc), trunc.max = ifelse(missing(trunc.max), NaN, trunc.max), hist = x)
}
