qpoig <- function(p, frac, rate, shape, S=30, lower.tail=TRUE, log.p=FALSE) {
  if (length(frac) > 1 | length(rate) > 1 | length(shape) > 1) stop("vectorization of parameters is currently not implemented")
  if (log.p) p <- exp(p)
  if(!lower.tail) p <- 1 - p
  d<-NULL
  busca <- function(U1, U2){
    repeat{
      tt <- dpoig(U2, frac, rate, shape)
      U2 <- ifelse(tt>=U1, U2-1, U2)
      a1 <- U2
      U2 <- ifelse(tt<U1, U2+1, U2)
      a2 <- U2
      if(ppoig(min(a1, a2), frac, rate, shape) < U1 & U1 <= ppoig(max(a1, a2), frac, rate, shape)){
        return(max(a1, a2))
      }
    }
  }
  for (i in 1:length(p)){
    U1 <- p[i]
    U2 <- round(runif(1, min=1, max=S))
    if(U1 <= ppoig(0, frac, rate, shape)){
      d[i] <- 0
    } else if(U1 >= 0.999999999999999999){
      d[i] <- Inf
    } else{
      d[i] <- busca(U1, U2)
    }
  }
  return(d)
}
