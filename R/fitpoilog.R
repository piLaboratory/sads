fitpoilog <- function(x, trunc = 0, ...){
  dots <- list(...)
  if (!missing(trunc)){
    if (min(x)<=trunc) stop("truncation point should be lower than the lowest data value")
    else{
      if(trunc==0){
        pl.par <- poilogMLE(x, startVals = c(mu = mean(log(x)) + log(0.5), sig = sd(log(x))), zTrunc = TRUE)$par
      }
      else pl.par <- poilogMLE(x, startVals = c(mu = mean(log(x)) + log(0.5), sig = sd(log(x))))$par
      LL <- function(mu, sig) -sum(dtrunc("poilog", x = x, coef = list(mu = mu, sig = sig), trunc = trunc, log = TRUE))
      result <-  mle2(LL, start = as.list(pl.par), data = list(x = x), ...)
    }
  }
  if (missing(trunc)){
    pl.par <- poilogMLE(x, startVals = c(mu = mean(log(x)) + log(0.5), sig = sd(log(x))), zTrunc = FALSE)$par
    LL <- function(mu, sig) -sum(dpoilog(x, mu, sig, log = TRUE))
    result <-  mle2(LL, start = as.list(pl.par), data = list(x = x), ...)
  }
  new("fitsad", result, sad="poilog", distr = "D", trunc = ifelse(missing(trunc), NaN, trunc))
}
