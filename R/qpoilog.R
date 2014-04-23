qpoilog<-function(p, mu, sig, S = 30, lower.tail = TRUE, log.p = FALSE){
  if (length(mu) > 1 | length(sig) > 1) stop("vectorization of mu and sig is currently not implemented")
  if (!all(is.finite(c(mu, sig)))) stop("all parameters should be finite")
  if (sig <= 0) stop("sig must be larger than zero")
  if (log.p) p <- exp(p)
  if(!lower.tail) p <- 1 - p
  d<-NULL
  busca <- function(U1, U2){
    repeat{
      tt <- ppoilog(U2, mu, sig)
      U2 <- ifelse(tt>=U1, U2-1, U2)
      a1 <- U2
      U2 <- ifelse(tt<U1, U2+1, U2)
      a2 <- U2
      if(ppoilog(min(a1, a2), mu, sig) < U1 & U1 <= ppoilog(max(a1, a2), mu, sig)){
        return(max(a1, a2))
      }
    }
  }
  for (i in 1:length(p)){
    U1 <- p[i]
    U2 <- round(runif(1, min=1, max=S))
    if(U1 <= ppoilog(0, mu, sig)){
      d[i] <- 0
    } else if(U1 >= 0.999999999999999999){
      d[i] <- Inf
    } else{
      d[i] <- busca(U1, U2)
    }
  }
  return(d)
}