fitpower <- function(x, trunc, start.value, upper = 20, ...){
  if (!missing(trunc)){
    if (min(x)<=trunc) stop("truncation point should be lower than the lowest data value")
  }
  if (missing(start.value)){
    gamahat <- function(ga, xvec) eq <- -sum(log(xvec)) - zeta(ga, deriv =1)*length(xvec)/zeta(ga)
    shat <- uniroot(gamahat, interval=c(1.01, upper), xvec = x)$root
  } else{
    shat <- start.value
  }
  if (missing(trunc)){
    LL <- function(s) -sum(dpower(x, s, log = TRUE))
  } else{
    LL <- function(s) -sum(dtrunc("power", x = x, coef = s, trunc = trunc, log = TRUE))
  }
  result <- mle2(LL, start = list(s = shat), data = list(x = x), method = "Brent", lower = 1, upper = upper, ...)
  if(abs(as.numeric(result@coef) - upper) < 0.0000001) warning("mle equal to upper bound provided. \n Try value for the 'upper' arguent")
  new("fitsad", result, sad = "power", distr = "D", trunc = ifelse(missing(trunc), NaN, trunc))
}
