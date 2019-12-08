fitlnormC <- function(x, trunc, start.value, ...){
    dots <- list(...)
    if (any(x$breaks < 0)) stop ("All x must be positive")
    if(missing(start.value)){
        y <- rep(x$mids, x$counts)
        meanlog <- mean(log(y))
        sdlog <- sd(log(y))
    }
    else{
        meanlog <- start.value[1]
        sdlog <-start.value[2]
    }
    if (missing(trunc)){
        LL <- function(meanlog, sdlog) -trueLL(x, dist = "lnorm", coef = list(meanlog = meanlog, sdlog = sdlog))
    }
    else {
        LL <- function(meanlog, sdlog) -trueLL(x, dist = "lnorm", coef = list(meanlog = meanlog, sdlog = sdlog), trunc = trunc)
    }  
    result <- do.call("mle2", c(list(LL, start = list(meanlog = meanlog, sdlog = sdlog)), dots))
    new("fitsadC", result, sad="lnorm", trunc = ifelse(missing(trunc), NaN, trunc), hist = x) 
}
