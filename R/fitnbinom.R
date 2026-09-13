fitnbinom <- function(x, trunc=0, trunc.max = NULL, start.value, ...){
  dots <- list(...)
  if ((any(x <= 0) & !is.null(trunc)) | any(!is.wholenumber(x))) stop ("All x must be positive integers")
  if (!is.null(trunc)){
    if (min(x)<=trunc) stop("truncation point should be lower than the lowest data value")
  }
  if (!is.null(trunc.max)){
    if (max(x)>trunc.max) stop("trunc.max should not be lower than the highest data value")
  }
  if(missing(start.value)){ 
    muhat <- length(x)/(length(x) + mean(x))
    sizehat <- muhat*mean(x) 
  }
  else{
    sizehat <- start.value[[1]]
    muhat <- start.value[[2]]
  }
  trunc.args <- list()
  if (!is.null(trunc)) trunc.args$trunc <- trunc
  if (!is.null(trunc.max)) trunc.args$trunc.max <- trunc.max
  if (length(trunc.args) > 0){
    LL <- function(size, mu) -sum(do.call(dtrunc, c(list("nbinom", x = x, coef = list(size=size, mu=mu), log = TRUE), trunc.args)))
  } else{
    LL <- function(size, mu) -sum(dnbinom(x, size=size, mu=mu, log = TRUE))
  }
  result <- do.call("mle2", c(list(LL, start = list(size = sizehat, mu = muhat), data = list(x = x)), dots))
  new("fitsad", result, sad="nbinom", distr = distr.depr, trunc = ifelse(is.null(trunc), NaN, trunc), trunc.max = ifelse(is.null(trunc.max), NaN, trunc.max))
}
