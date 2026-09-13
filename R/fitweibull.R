fitweibull <- function(x, trunc, trunc.max, start.value, ...){
  dots <- list(...)
  if (any(x <= 0)) stop ("All x must be positive")
  if (!missing(trunc)){
    if (min(x)<=trunc) stop("truncation point should be lower than the lowest data value")
  }
  if (!missing(trunc.max)){
    if (max(x)>trunc.max) stop("trunc.max should not be lower than the highest data value")
  }
  if (missing(start.value)) {
	  f <- function(shape, x){
		  n <- length(x)
		  (1 / shape) + (1 / n) * sum(log(x)) - (sum((x ^ shape) * log(x)) / sum(x ^ shape))
	  }
	  ka <- uniroot(f, interval = c(0.0000001, 10), x=x)$root
	  theta <- ((1 / length(x)) * sum(x ^ ka)) ^ (1 / ka)
  } else{
    ka <- start.value[1]
    theta <-start.value[2]
  }
  trunc.args <- list()
  if (!missing(trunc)) trunc.args$trunc <- trunc
  if (!missing(trunc.max)) trunc.args$trunc.max <- trunc.max
  if (length(trunc.args) > 0){
    LL <- function(shape, scale) -sum(do.call(dtrunc, c(list("weibull", x = x, coef = list(shape, scale), log = TRUE), trunc.args)))
  } else {
    LL <- function(shape, scale) -sum(dweibull(x, shape, scale, log = TRUE))
  }  
  result <- do.call("mle2", c(list(LL, start = list(shape = ka, scale = theta), data = list(x = x)), dots))
  new("fitsad", result, sad="weibull", distr = distr.depr, trunc = ifelse(missing(trunc), NaN, trunc), trunc.max = ifelse(missing(trunc.max), NaN, trunc.max)) 
}
