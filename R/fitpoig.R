fitpoig <- function(x, trunc = 0, start.value, ...){
  if (missing(start.value)){
	  ## NEEDS BETTER START VALUE!!
  	  frac = 0.5
	  rate = 1/mean(x)
	  shape = 1
  }
  else{
    frac <- start.value[1]
    rate <- start.value[2]
    shape <- start.value[3]
  }
  if (!missing(trunc)){
    if (min(x)<=trunc) stop("truncation point should be lower than the lowest data value")
    else{
      LL <- function(frac, rate, shape) -sum(dtrunc("poig", x = x, coef = list(frac = frac, rate = rate, shape=shape), trunc = trunc, log = TRUE))
      result <- mle2(LL, start = list(frac = frac, rate=rate, shape=shape), data = list(x = x), ...)
    }
  }
  if (missing(trunc)){
    LL <- function(frac, rate, shape) -sum(dpoig(x, frac, rate, shape, log = TRUE))
    result <- mle2(LL, start = list(frac=frac, rate=rate, shape=shape), data = list(x = x), ...)
  }
  new("fitsad", result, sad = "poig", distr = "D", trunc = ifelse(missing(trunc), NaN, trunc))
}
