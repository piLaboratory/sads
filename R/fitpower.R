fitpower <- function(x, trunc, trunc.max, start.value, upper = 20, ...){
	dots <-list(...)
  if (any(x <= 0) | any(!is.wholenumber(x))) stop ("All x must be positive integers")
  if (!missing(trunc)){
    if (min(x)<=trunc) stop("truncation point should be lower than the lowest data value")
  }
  if (!missing(trunc.max)){
    if (max(x)>trunc.max) stop("trunc.max should not be lower than the highest data value")
  }
  if (missing(start.value)){
    gamahat <- function(ga, xvec) eq <- -sum(log(xvec)) - zeta(ga, deriv =1)*length(xvec)/zeta(ga)
    shat <- uniroot(gamahat, interval=c(1.01, upper), xvec = x)$root
  } else{
    shat <- start.value
  }
  trunc.args <- list()
  if (!missing(trunc)) trunc.args$trunc <- trunc
  if (!missing(trunc.max)) trunc.args$trunc.max <- trunc.max
  if (length(trunc.args) > 0){
    LL <- function(s) -sum(do.call(dtrunc, c(list("power", x = x, coef = s, log = TRUE), trunc.args)))
  } else{
    LL <- function(s) -sum(dpower(x, s, log = TRUE))
  }
  result <- do.call("mle2", c(list(LL, start = list(s = shat), data = list(x = x), method = "Brent", lower = 1, upper = upper), dots))
  if(abs(as.numeric(result@coef) - upper) < 0.0000001) warning("mle equal to upper bound provided. \n Try value for the 'upper' arguent")
  new("fitsad", result, sad = "power", distr = distr.depr, trunc = ifelse(missing(trunc), NaN, trunc), trunc.max = ifelse(missing(trunc.max), NaN, trunc.max))
}
