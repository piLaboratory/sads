ptrunc <- function(f, q, trunc, trunc.max, coef, lower.tail=TRUE, log.p=FALSE){
  pf <- get(paste("p", f, sep = ""), mode = "function")
  has.min <- !missing(trunc)
  has.max <- !missing(trunc.max)
  if (has.min && has.max && trunc >= trunc.max)
    stop("trunc should be lower than trunc.max")
  if (!has.min && !has.max){
    tt <- do.call(pf, c(list(q = q), coef, lower.tail = lower.tail))
  } else{
    qc <- q
    if (has.min) qc <- pmax(qc, trunc)
    if (has.max) qc <- pmin(qc, trunc.max)
    Ga <- if (has.min) do.call(pf, c(list(q = trunc), coef)) else 0
    Gb <- if (has.max) do.call(pf, c(list(q = trunc.max), coef)) else 1
    Gqc <- do.call(pf, c(list(q = qc), coef))
    if (lower.tail){
      tt <- (Gqc - Ga)/(Gb - Ga)
    } else{
      tt <- (Gb - Gqc)/(Gb - Ga)
    }
  }
  if(log.p)return(log(tt)) else return(tt)
}
