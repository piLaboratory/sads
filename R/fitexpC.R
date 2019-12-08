fitexpC <- function(x, trunc = NULL, start.value, ...){
    dots <- list(...)
    if (any(x$breaks < 0)) stop ("All x must be positive")
    if (missing(start.value)){
        y <- rep(x$mids, x$counts)
        phat <- 1/(mean(y))
    }
    else{
        phat <- start.value
    }
    if(!"method" %in% names(dots)){
        dots$method <- "Brent"
        if(!"lower" %in% names(dots)) dots$lower=max(c(phat/10, 1e-8))
        if(!"upper" %in% names(dots)) dots$upper=min(c(phat*10, 0.99))
    }
    if (is.null(trunc)){
        LL <- function(rate) -trueLL(x, dist = "exp", coef = list(rate = rate))
    }
    else{
        LL <- function(rate) -trueLL(x, dist = "exp", coef = list(rate = rate), trunc = trunc)
    }
    result <- do.call("mle2", c(list(LL, start = list(rate = phat)), dots))  
    new("fitsadC", result, sad = "exp", trunc = ifelse(is.null(trunc), NaN, trunc), hist = x)
}
