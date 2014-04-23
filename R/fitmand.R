fitmand <- function(x, trunc, start.value, ...){
  if(class(x)!="rad") rad.tab <- rad(x)
  else rad.tab <- x
  N <- max(rad.tab$rank)
  y <- rep(rad.tab$rank, rad.tab$abund)
  dots <- list(...)
  if (!missing(trunc)){
    if (min(y)<=trunc) stop("truncation point should be lower than the lowest data value")
  }
  if(missing(start.value)){
    shat <- 2
    vhat <- 30
  }
  else{
    shat <- start.value[1]
    vhat <- start.value[2]
  }
  if (missing(trunc)){
    LL <- function(N, s, v) -sum(dmand(y, N = N, s, v, log = TRUE))
  }
  else{
    LL <- function(N, s, v) -sum(dtrunc("mand", x = y, coef = list(N = N, s = s, v = v), trunc = trunc, log = TRUE))
  }
  result <- mle2(LL, start = list(s = shat, v = vhat, N=N), data = list(x = y), fixed=list(N=N), ...)
  new("fitrad", result, rad="mand", distr = "D", trunc = ifelse(missing(trunc), NaN, trunc), rad.tab=rad.tab)
}
