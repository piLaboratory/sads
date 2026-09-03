fitgs <- function(x, trunc, trunc.max, start.value, ...){
  if (any(x <= 0)) stop ("All x must be positive")
	dots <-list(...)
  ##if(class(x)!="rad") rad.tab <- rad(x)
  if(!inherits(x, "rad")) rad.tab <- rad(x)
  else rad.tab <- x
  y <- rep(rad.tab$rank, rad.tab$abund)
  S <- length(rad.tab$abund)
  if (missing(start.value)) {
    khat <- 1 - (min(rad.tab$abund) / max(rad.tab$abund)) ^ (1 / (S-1))
  } else {
    khat <- start.value
  }

  if (!missing(trunc)){
    if (min(x)<=trunc) stop("truncation point should be lower than the lowest data value")
  }
  if (!missing(trunc.max)){
    if (max(x)>trunc.max) stop("trunc.max should not be lower than the highest data value")
  }
  trunc.args <- list()
  if (!missing(trunc)) trunc.args$trunc <- trunc
  if (!missing(trunc.max)) trunc.args$trunc.max <- trunc.max
  if (length(trunc.args) > 0){
    LL <- function(S, k) -sum(do.call(dtrunc, c(list("gs", x = y, coef = list( k = k, S = S), log = TRUE), trunc.args)))
  } else {
    LL <- function(S, k) -sum(dgs(y, k, S, log = TRUE))
  }
  result <- do.call("mle2", c(list(LL, start = list(k=khat), data = list(x = y), fixed=list(S=S), method = "Brent", lower = 1e-16, upper = 1-1e-16), dots))
  new("fitrad", result, rad = "gs", distr = distr.depr, trunc = ifelse(missing(trunc), NaN, trunc), trunc.max = ifelse(missing(trunc.max), NaN, trunc.max), rad.tab=rad.tab)
}
