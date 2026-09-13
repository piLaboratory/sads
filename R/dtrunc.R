dtrunc <- function(f, x, trunc, trunc.max, coef, log = FALSE){
  pf <- get(paste("p", f, sep=""), mode = "function")
  df <- get(paste("d", f, sep=""), mode = "function")
  has.min <- !missing(trunc)
  has.max <- !missing(trunc.max)
  if (has.min && has.max && trunc >= trunc.max)
    stop("trunc should be lower than trunc.max")
  if (!has.min && !has.max){
    tt <- do.call(df, c(list(x = x), coef))
  } else{
    tt <- rep(0, length(x))
    keep <- rep(TRUE, length(x))
    if (has.min) keep <- keep & (x > trunc)
    if (has.max) keep <- keep & (x <= trunc.max)
    denom <- 1
    if (has.max) denom <- do.call(pf, c(list(q = trunc.max), coef))
    if (has.min) denom <- denom - do.call(pf, c(list(q = trunc), coef))
    tt[keep] <- do.call(df, c(list(x = x[keep]), coef)) / denom
  }
  if (log) tt <- log(tt)
  return(tt)
}
