fitls <- function(x, trunc, trunc.max, start.value, upper = length(x), ...){
	dots <- list(...)
  if (any(x <= 0) | any(!is.wholenumber(x))) stop ("All x must be positive integers")
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
  }
  if (!missing(trunc.max)){
    if (max(x)>trunc.max) stop("trunc.max should not be lower than the highest data value")
  }
  trunc.args <- list()
  if (!missing(trunc)) trunc.args$trunc <- trunc
  if (!missing(trunc.max)) trunc.args$trunc.max <- trunc.max
  if (length(trunc.args) > 0){
    LL <- function(N, alpha) -sum(do.call(dtrunc, c(list("ls", x = x, coef = list(N = N, alpha = alpha), log = TRUE), trunc.args)))
  } else {
    LL <- function(N, alpha) -sum(dls(x, N, alpha, log = TRUE))
  }
  result <- do.call("mle2", c(list(LL, start = list(alpha = alfa), data = list(x = x), fixed=list(N=N), method = "Brent", lower = 0, upper = upper), dots))
  if(abs(as.numeric(result@coef) - upper) < 0.0000001)
    warning("mle equal to upper bound provided. \n Try new value for the 'upper' argument")
  new("fitsad", result, sad = "ls", distr = distr.depr, trunc = ifelse(missing(trunc), NaN, trunc), trunc.max = ifelse(missing(trunc.max), NaN, trunc.max))
}
