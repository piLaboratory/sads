fitmand <- function(x, trunc, trunc.max, start.value, ...){
  if (any(x <= 0)) stop ("All x must be positive")
  ##if(class(x)!="rad") rad.tab <- rad(x)
  if(!inherits(x, "rad")) rad.tab <- rad(x)
  else rad.tab <- x
  N <- max(rad.tab$rank)
  y <- rep(rad.tab$rank, rad.tab$abund)
  dots <- list(...)
  if (!missing(trunc)){
    if (min(y)<=trunc) stop("truncation point should be lower than the lowest data value")
  }
  if (!missing(trunc.max)){
    if (max(y)>trunc.max) stop("trunc.max should not be lower than the highest data value")
  }
  if(missing(start.value)){
	x75 <- rad.tab[(floor(dim(rad.tab)[1]/4)):dim(rad.tab)[1],]
    x75 <- x75[x75$abund > 1, ]
	shat <- - coef(lm(log(abund)~log(rank), data=x75))[2]
    vhat <- 30
  }
  else{
    shat <- start.value[1]
    vhat <- start.value[2]
  }
  trunc.args <- list()
  if (!missing(trunc)) trunc.args$trunc <- trunc
  if (!missing(trunc.max)) trunc.args$trunc.max <- trunc.max
  if (length(trunc.args) > 0){
    LL <- function(N, s, v) -sum(do.call(dtrunc, c(list("mand", x = y, coef = list(N = N, s = s, v = v), log = TRUE), trunc.args)))
  } else{
    LL <- function(N, s, v) -sum(dmand(y, N = N, s=s, v=v, log = TRUE))
  }
  result <- do.call("mle2", c(list(LL, start = list(s = shat, v = vhat), fixed=list(N=N), data = list(x = y)), dots))
  new("fitrad", result, rad="mand", distr = distr.depr, trunc = ifelse(missing(trunc), NaN, trunc), trunc.max = ifelse(missing(trunc.max), NaN, trunc.max), rad.tab=rad.tab)
}
