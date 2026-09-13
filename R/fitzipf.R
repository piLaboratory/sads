fitzipf <- function(x, N, trunc, trunc.max, start.value, upper = 20, ...){
  if (any(x <= 0)) stop ("All x must be positive")
  ##if(class(x)!="rad") rad.tab <- rad(x)
  if(!inherits(x, "rad")) rad.tab <- rad(x)
  else rad.tab <- x
  y <- rep(rad.tab$rank, rad.tab$abund)
  dots <- list(...)
  if (!missing(trunc)){
    if (min(y)<=trunc) stop("truncation point should be lower than the lowest rank")
  }
  if (!missing(trunc.max)){
    if (max(y)>trunc.max) stop("trunc.max should not be lower than the highest rank")
  }
  if(missing(N)){
    N <- max(rad.tab$rank)
  }
  if(missing(start.value)){
    p <- rad.tab$abund/sum(rad.tab$abund)
    lzipf <- function(s, N) -s*log(1:N) - log(sum(1/(1:N)^s))
    opt.f <- function(s) sum((log(p) - lzipf(s, length(p)))^2)
    opt <- optimize(opt.f, c(0.5, length(p)))
    sss <- opt$minimum
  }
  else{
    sss <- start.value
  }
  trunc.args <- list()
  if (!missing(trunc)) trunc.args$trunc <- trunc
  if (!missing(trunc.max)) trunc.args$trunc.max <- trunc.max
  if (length(trunc.args) > 0){
    LL <- function(N, s) -sum(do.call(dtrunc, c(list("zipf", x = y, coef = list(N = N, s = s), log = TRUE), trunc.args)))
  }
  else{
    LL <- function(N, s) -sum(dzipf(y, N=N, s=s, log = TRUE))
  }
  result <- do.call("mle2", c(list(LL, start = list(s = sss), data = list(x = y), fixed=list(N=N), method = "Brent", lower = 0, upper = upper), dots))
  if(abs(as.numeric(result@coef) - upper) < 0.001)
    warning("mle equal to upper bound provided. \n Try increase value for the 'upper' argument")
  new("fitrad", result, rad="zipf", distr = distr.depr, trunc = ifelse(missing(trunc), NaN, trunc), trunc.max = ifelse(missing(trunc.max), NaN, trunc.max), rad.tab=rad.tab)
}
