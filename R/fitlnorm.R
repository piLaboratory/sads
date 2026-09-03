fitlnorm <- function(x, trunc, trunc.max, start.value, ...){
  dots <- list(...)
  if (any(x <= 0)) stop ("All x must be positive")
  if (!missing(trunc)){
    if (min(x)<=trunc) stop("truncation point should be lower than the lowest data value")
  }
  if (!missing(trunc.max)){
    if (max(x)>trunc.max) stop("trunc.max should not be lower than the highest data value")
  }
  if(missing(start.value)){
    meanlog <- mean(log(x))
    sdlog <- sd(log(x))
  } else{
    meanlog <- start.value[1]
    sdlog <-start.value[2]
  }
  trunc.args <- list()
  if (!missing(trunc)) trunc.args$trunc <- trunc
  if (!missing(trunc.max)) trunc.args$trunc.max <- trunc.max
  if (length(trunc.args) > 0){
    LL <- function(meanlog, sdlog) -sum(do.call(dtrunc, c(list("lnorm", x, coef = list(meanlog = meanlog, sdlog = sdlog), log = TRUE), trunc.args)))
  } else {
    LL <- function(meanlog, sdlog) -sum(dlnorm(x, meanlog, sdlog, log = TRUE))
  }
  result <- do.call("mle2", c(list(LL, start = list(meanlog = meanlog, sdlog = sdlog), data = list(x = x)), dots))
  new("fitsad", result, sad="lnorm", distr = distr.depr, trunc = ifelse(missing(trunc), NaN, trunc), trunc.max = ifelse(missing(trunc.max), NaN, trunc.max))
}
