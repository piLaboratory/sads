fitexpC <- function(x, trunc = NULL, trunc.max = NULL, start.value, ...){
    dots <- list(...)
    if (any(x$breaks < 0)) stop ("All x must be positive")
    if (!is.null(trunc.max)){
        if (max(x$breaks)>trunc.max) stop("trunc.max should not be lower than the highest data value")
    }
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
    trunc.args <- list()
    if (!is.null(trunc)) trunc.args$trunc <- trunc
    if (!is.null(trunc.max)) trunc.args$trunc.max <- trunc.max
    if (length(trunc.args) > 0){
        LL <- function(rate) -do.call(trueLL, c(list(x, dist = "exp", coef = list(rate = rate)), trunc.args))
    }
    else{
        LL <- function(rate) -trueLL(x, dist = "exp", coef = list(rate = rate))
    }
    result <- do.call("mle2", c(list(LL, start = list(rate = phat)), dots))  
    new("fitsadC", result, sad = "exp", trunc = ifelse(is.null(trunc), NaN, trunc), trunc.max = ifelse(is.null(trunc.max), NaN, trunc.max), hist = x)
}
