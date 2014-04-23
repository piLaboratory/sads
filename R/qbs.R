qbs<-function(p, N, S, lower.tail = TRUE, log.p = FALSE){
  if (length(N) > 1) stop("vectorization of N is not implemented")
  if (length(S) > 1) stop("vectorization of S is not implemented")
  if (!all(is.finite(c(N, S)))) stop("all parameters should be finite")
  if (N <= 0)  stop("N must be larger than zero")
  if (S <= 0)  stop("S must be larger than zero")
  d<-NULL
  if (log.p) p <- exp(p)
  if(!lower.tail) p <- 1 - p
  busca <- function(U1, U2){
    repeat{
      tt <- pbs(U2, N, S)
      U2 <- ifelse(tt>=U1, U2-1, U2)
      a1 <- U2
      U2 <- ifelse(tt<U1, U2+1, U2)
      a2 <- U2
      if(pbs(min(a1, a2), N, S) < U1 & U1 <= pbs(max(a1, a2), N, S)){
        return(max(a1, a2))
      }
    }
  }
  for (i in 1:length(p)){
    U1 <- p[i]
    U2 <- round(runif(1, min=1, max=N))
    if(U1 <= pbs(1, N, S)){
      d[i] <- 1
    } else if (U1 >= 0.999999999999999999){
      d[i] <- Inf
    } else{
      d[i] <- busca(U1, U2)
    }
  }
  return(d)
}
