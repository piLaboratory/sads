fitmzsm <- function(x, trunc, trunc.max, start.value, upper = length(x), ...){
  dots <- list(...)
  if (any(x <= 0) | any(!is.wholenumber(x))) stop ("All x must be positive integers")
  if(sum(x)<100) warning("\n small sample size (J<100); \n mzsm may not be a good approximation")
  if (!missing(trunc)){
    if (min(x)<=trunc) stop("truncation point should be lower than the lowest data value")
  }
  if (!missing(trunc.max)){
    if (max(x)>trunc.max) stop("trunc.max should not be lower than the highest data value")
  }
  if(missing(start.value)){
    thetahat <- length(x)
  }
  else{
    thetahat <- start.value
  }
  trunc.args <- list()
  if (!missing(trunc)) trunc.args$trunc <- trunc
  if (!missing(trunc.max)) trunc.args$trunc.max <- trunc.max
  if (length(trunc.args) > 0){
      LL <- function(J, theta) -sum(do.call(dtrunc, c(list("mzsm", x=x, coef = list(J = J, theta = theta), log = TRUE), trunc.args)))
    }
  else {
    LL <- function(J, theta) -sum(dmzsm(x, J=J, theta = theta, log = TRUE))
  }
  result <- do.call("mle2", c(list(LL, start = list(theta = thetahat), fixed=list(J=sum(x)), data = list(x = x), method ="Brent", lower=0.001, upper=upper), dots))
  if(abs(as.numeric(result@coef) - upper) < 0.0000001) warning("mle equal to upper bound provided. \n Try value for the 'upper' arguent")
  new("fitsad", result, sad="mzsm", distr = distr.depr, trunc = ifelse(missing(trunc), NaN, trunc), trunc.max = ifelse(missing(trunc.max), NaN, trunc.max))
}
