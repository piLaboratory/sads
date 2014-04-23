fitls <- function(x, trunc, start.value, upper = length(x), ...){
  S <- length(x)
  N <- sum(x)
  if (missing(start.value)){
    f1 <- function(a) {
      S + a*log((a/(a + N)))
    }
    sol <- uniroot(f1, interval = c(1/N, N))
    alfa <- sol$root
    X <- N/(N + alfa)
  }
  else{
    alfa <- start.value
  }
  if (!missing(trunc)){
    if (min(x)<=trunc) stop("truncation point should be lower than the lowest data value")
    else{
      LL <- function(alpha) -sum(dtrunc("ls", x = x, coef = list(N = N, alpha = alpha), trunc = trunc, log = TRUE))
      result <- mle2(LL, start = list(alpha = alfa), data = list(x = x), method = "Brent", lower = 0, upper = upper, ...)
    }
  }
  if (missing(trunc)){
    LL <- function(alpha) -sum(dls(x, N, alpha, log = TRUE))
    result <- mle2(LL, start = list(alpha = alfa), data = list(x = x), method = "Brent", lower = 0, upper = upper, ...)
  }
  if(abs(as.numeric(result@coef) - upper) < 0.0000001)
    warning("mle equal to upper bound provided. \n Try new value for the 'upper' argument")
  new("fitsad", result, sad = "ls", distr = "D", trunc = ifelse(missing(trunc), NaN, trunc))
}
