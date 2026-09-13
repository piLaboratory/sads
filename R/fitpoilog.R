fitpoilog <- function(x, trunc = 0, trunc.max = NULL, ...){
    dots <- list(...)
  if ((any(x <= 0)&!is.null(trunc)) | any(!is.wholenumber(x))) stop ("All x must be positive integers")
    if (!is.null(trunc.max)){
        if (max(x)>trunc.max) stop("trunc.max should not be lower than the highest data value")
    }
    if (!is.null(trunc)){
        if (min(x)<=trunc) stop("truncation point should be lower than the lowest data value")
        else{
            if(trunc==0){
                pl.par <- poilog::poilogMLE(x, startVals = c(mu = mean(log(x)) + log(0.5), sig = sd(log(x))), zTrunc = TRUE)$par
            }
            else pl.par <- poilog::poilogMLE(x, startVals = c(mu = mean(log(x)) + log(0.5), sig = sd(log(x))))$par
            trunc.args <- list(trunc = trunc)
            if (!is.null(trunc.max)) trunc.args$trunc.max <- trunc.max
            LL <- function(mu, sig) -sum(do.call(dtrunc, c(list("poilog", x = x, coef = list(mu = mu, sig = sig), log = TRUE), trunc.args)))
        }
    }
    if (is.null(trunc)){
        pl.par <- poilogMLE(x, startVals = c(mu = mean(log(x+0.1)) + log(0.5), sig = sd(log(x+0.1))), zTrunc = FALSE)$par
        if (!is.null(trunc.max)){
            LL <- function(mu, sig) -sum(dtrunc("poilog", x = x, coef = list(mu = mu, sig = sig), trunc.max = trunc.max, log = TRUE))
        } else{
            LL <- function(mu, sig) -sum(dpoilog(x, mu, sig, log = TRUE))
        }
    }
    result <- do.call("mle2", c(list(LL, start = as.list(pl.par), data = list(x = x)), dots))
    new("fitsad", result, sad="poilog", distr = distr.depr, trunc = ifelse(is.null(trunc), NaN, trunc), trunc.max = ifelse(is.null(trunc.max), NaN, trunc.max))
}
