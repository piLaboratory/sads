fitpoix <- function(x, trunc = 0, start.value, ...){
  if (missing(start.value)){
	  ## NEEDS BETTER START VALUE!!
  	  frac = 0.5
	  rate = 1/mean(x)
  }
  else{
    frac <- start.value[1]
    rate <- start.value[2]
  }
  if (!missing(trunc)){
    if (min(x)<=trunc) stop("truncation point should be lower than the lowest data value")
    else{
      LL <- function(frac, rate) -sum(dtrunc("poix", x = x, coef = list(frac = frac, rate = rate), trunc = trunc, log = TRUE))
      result <- mle2(LL, start = list(frac = frac, rate=rate), data = list(x = x), ...)
    }
  }
  if (missing(trunc)){
    LL <- function(frac, rate) -sum(dpoix(x, frac, rate, log = TRUE))
    result <- mle2(LL, start = list(frac=frac, rate=rate), data = list(x = x), ...)
  }
  new("fitsad", result, sad = "poix", distr = "D", trunc = ifelse(missing(trunc), NaN, trunc))
}
