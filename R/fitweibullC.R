fitweibullC <- function(x, trunc, start.value, ...){
    dots <- list(...)
    if (any(x$breaks < 0)) stop ("All x must be positive")
    if (missing(start.value)) {
        f <- function(shape, x){
            n <- length(x)
            (1 / shape) + (1 / n) * sum(log(x)) - (sum((x ^ shape) * log(x)) / sum(x ^ shape))
        }
        y <- rep(x$mids, x$counts)
        ka <- uniroot(f, interval = c(0.0000001, 10), x=y)$root
        theta <- ((1 / length(y)) * sum(y ^ ka)) ^ (1 / ka)
    } else{
        ka <- start.value[1]
        theta <-start.value[2]
    }
    if (missing(trunc)){
       LL <- function(shape, scale) -trueLL(x, dist = "weibull", coef = list( shape = shape, scale = scale)) 
    }
    else {
      LL <- function(shape, scale) -trueLL(x, dist = "weibull", coef = list( shape = shape, scale = scale), trunc = trunc)   
    }  
    result <- do.call("mle2", c(list(LL, start = list(shape = ka, scale = theta)), dots))
    new("fitsadC", result, sad="weibull", trunc = ifelse(missing(trunc), NaN, trunc), hist = x) 
}
