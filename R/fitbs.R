fitbs <- function(x, trunc, ...){
  s <- length(x)
  n <- sum(x)
  if (!missing(x)){
    if (!missing(trunc)){
      if (min(x)<=trunc) stop("truncation point should be lower than the lowest data value")
      else{
        LL <- function(N,S) -sum(dtrunc("bs", x = x, coef = list(N = N, S = S), trunc = trunc, log = TRUE))
        result <- mle2(LL, data = list(x = x), fixed=list(N=n, S=s), eval.only=TRUE, ...)
      }
    }
    if (missing(trunc)){
      LL <- function(N,S) -sum(dbs(x = x, N = N, S = S, log = TRUE))
      result <- mle2(LL, data = list(x = x), fixed=list(N=n, S=s), eval.only=TRUE, ...)
    }
    new("fitsad", result, sad = "bs", distr = "D", trunc = ifelse(missing(trunc), NaN, trunc))
  }
}
