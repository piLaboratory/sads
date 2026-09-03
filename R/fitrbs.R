fitrbs <- function(x, trunc, trunc.max, ...){
  if (any(x <= 0)) stop ("All x must be positive")
	dots <- list(...)
  ##if(class(x)!="rad") rad.tab <- rad(x)
  if(!inherits(x, "rad")) rad.tab <- rad(x)
    else rad.tab <- x
  y <- rep(rad.tab$rank, rad.tab$abund)
  N <- sum(rad.tab$abund)
  S <- length(rad.tab$abund)
  if (!missing(trunc)){
    if (min(y)<=trunc) stop("truncation point should be lower than the lowest rank")
  }
  if (!missing(trunc.max)){
    if (max(y)>trunc.max) stop("trunc.max should not be lower than the highest rank")
  }
  if(missing(N)){
    N <- max(rad.tab$rank)
  }
  trunc.args <- list()
  if (!missing(trunc)) trunc.args$trunc <- trunc
  if (!missing(trunc.max)) trunc.args$trunc.max <- trunc.max
  if (length(trunc.args) > 0){
    LL <- function(N, S) -sum(do.call(dtrunc, c(list("rbs", x = y, coef = list(N = N, S = S), log = TRUE), trunc.args)))
  }
  else{
    LL <- function(N, S) -sum(drbs(y, N, S, log = TRUE))
  }
  result <- do.call("mle2", c(list(LL, start = list(N=N, S = S), data = list(x = y), fixed=list(N=N, S=S), eval.only=T), dots))
  new("fitrad", result, rad="rbs", distr = distr.depr, trunc = ifelse(missing(trunc), NaN, trunc), trunc.max = ifelse(missing(trunc.max), NaN, trunc.max), rad.tab=rad.tab)
}
