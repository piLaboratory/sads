qtrunc <- function(f, p, trunc, trunc.max, coef, lower.tail = TRUE, log.p = FALSE){
  if(log.p) p <- exp(p)
  pf <- get(paste("p", f, sep = ""), mode = "function")
  qf <- get(paste("q", f, sep = ""), mode = "function")
  has.min <- !missing(trunc)
  has.max <- !missing(trunc.max)
  if (has.min && has.max && trunc >= trunc.max)
    stop("trunc should be lower than trunc.max")
  if (!has.min && !has.max){
    tt <- do.call(qf, c(list(p = p), coef, lower.tail = lower.tail))
  } else{
    Ga <- if (has.min) do.call(pf, c(list(q = trunc), coef, lower.tail = TRUE)) else 0
    Gb <- if (has.max) do.call(pf, c(list(q = trunc.max), coef, lower.tail = TRUE)) else 1
    if (lower.tail){
      aa <- Ga + p*(Gb - Ga)
    } else{
      aa <- Gb - p*(Gb - Ga)
    }
    tt <- do.call(qf, c(list(p = aa), coef, lower.tail = TRUE))
  }
  return(tt)
}
