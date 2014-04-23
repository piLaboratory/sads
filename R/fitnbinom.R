fitnbinom <- function(x, trunc=0, start.value, ...){
  dots <- list(...)
  if (!missing(trunc)){
    if (min(x)<=trunc) stop("truncation point should be lower than the lowest data value")
  }
  if(missing(start.value)){ #Olhar os chutes default
    muhat <- length(x)/(length(x) + mean(x))
    sizehat <- muhat*mean(x) 
  }
  else{
    sizehat <- start.value[[1]]
    muhat <- start.value[[2]]
  }
  if (missing(trunc)){
    LL <- function(size, mu) -sum(dnbinom(x, size=size, mu=mu, log = TRUE))
  } else{
    LL <- function(size, mu) -sum(dtrunc("nbinom", x = x, coef = list(size=size, mu=mu), trunc = trunc, log = TRUE))
  }
  result <- mle2(LL, start = list(size = sizehat, mu = muhat), data = list(x = x), ...)
  new("fitsad", result, sad="nbinom", distr = "D", trunc = ifelse(missing(trunc), NaN, trunc))
}
