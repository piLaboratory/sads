fitpareto <- function(x, trunc, trunc.max, start.value, upper = 20, ...){
  dots <- list(...)
  if (any(x <= 0)) stop ("All x must be positive")
  if (!missing(trunc)){
    if (min(x)<=trunc) stop("truncation point should be lower than the lowest data value")
  }
  if (!missing(trunc.max)){
    if (max(x)>trunc.max) stop("trunc.max should not be lower than the highest data value")
  }
  if(missing(start.value)){
    alpha <- length(x)/sum(log(x)-log(min(x))) ## actually the analytical mle of shape
  } else{
    alpha <- start.value[1]
  }
  trunc.args <- list()
  if (!missing(trunc)) trunc.args$trunc <- trunc
  if (!missing(trunc.max)) trunc.args$trunc.max <- trunc.max
  if (length(trunc.args) > 0){
    LL <- function(shape, scale) -sum(do.call(dtrunc, c(list("pareto", x = x,
                                             coef = list(shape = shape, scale = scale), log = TRUE), trunc.args)))
  } else {
    LL <- function(shape,scale) -sum(dpareto(x, shape, scale, log = TRUE))
  }
  ##result <- do.call("mle2", c(list(LL, start = list(shape = alpha, scale=min(x)), fixed=list(scale=min(x)),
  ##                                 data = list(x = x), method = "Brent", lower = 0, upper = upper), dots))
  ## With both parameters free, because the fit has two degrees of freedom
  result <- do.call("mle2", c(list(LL, start = list(shape = alpha, scale=min(x)),
                                   data = list(x = x), method = "L-BFGS-B", upper= c(shape = upper, scale=min(x))), dots))
  if(abs(as.numeric(result@coef[1]) - upper) < 0.001)
    warning("mle equal to upper bound provided. \n Try value for the 'upper' argument")
  new("fitsad", result, sad="pareto", distr = distr.depr, trunc = ifelse(missing(trunc), NaN, trunc), trunc.max = ifelse(missing(trunc.max), NaN, trunc.max))
}
