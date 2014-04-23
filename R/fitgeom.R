fitgeom <- function(x, trunc = 0, start.value, ...){
  if (!missing(trunc)){
    if (min(x)<=trunc) stop("truncation point should be lower than the lowest data value")
  }
  if (missing(start.value)){
    phat <- 1/(1 + mean(x))
  } else{
    phat <- start.value
  }
  if (missing(trunc)){
    LL <- function(prob) -sum(dgeom(x, prob, log = TRUE))
  } else{
    LL <- function(prob) -sum(dtrunc("geom", x = x, coef = prob, trunc = trunc, log = TRUE))
  }
  result <- mle2(LL, start = list(prob = phat), data = list(x = x), method = "Brent", lower = 0, upper = 1, ...)
  new("fitsad", result, sad = "geom", distr = "D", trunc = ifelse(missing(trunc), NaN, trunc))
}
