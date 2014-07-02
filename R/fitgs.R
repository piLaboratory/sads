fitgs <- function(x, trunc, ...){
  if(class(x)!="rad") rad.tab <- rad(x)
  else rad.tab <- x
  y <- rep(rad.tab$rank, rad.tab$abund)
  S <- length(rad.tab$abund)
  if (!missing(trunc)){
    if (min(x)<=trunc) stop("truncation point should be lower than the lowest data value")
    else{
      LL <- function(k) -sum(dtrunc("gs", x = y, coef = list( k = k, S = S),
                                          trunc = trunc, log = TRUE))
      }
  }
  if (missing(trunc)){
    LL <- function(k) -sum(dgs(y, k, S, log = TRUE))
  }
  result <- mle2(LL, start = list(k=0.01), data = list(x = y), method = "Brent", lower = 1e-16, upper = 1-1e-16, ...)
  new("fitrad", result, rad = "gs", distr = "D", trunc = ifelse(missing(trunc), NaN, trunc), rad.tab=rad.tab)
}
