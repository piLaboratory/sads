qls<-function(p, N, alpha, lower.tail = TRUE, log.p = FALSE){
  if (length(N) > 1) stop("vectorization of N is not implemented")
  if (!all(is.finite(c(N, alpha)))) stop("all parameters should be finite")
  if (N <= 0)  stop("N must be larger than zero")
  d<-NULL
  if (log.p) p <- exp(p)
  if(!lower.tail) p <- 1 - p
  busca <- function(U1, U2){
    repeat{
      tt <- pls(U2, N, alpha)
      U2 <- ifelse(tt>=U1, U2-1, U2)
      a1 <- U2
      U2 <- ifelse(tt<U1, U2+1, U2)
      a2 <- U2
      if(pls(min(a1, a2), N, alpha) < U1 & U1 <= pls(max(a1, a2), N, alpha)){
        return(max(a1, a2))
      }
    }
  }
  for (i in 1:length(p)){
    U1 <- p[i]
    U2 <- round(runif(1, min=1, max=N))
    if(U1 <= pls(1, N, alpha)){
      d[i] <- 1
    } else if (U1 >= 0.999999999999999999){
      d[i] <- Inf
    } else{
      d[i] <- busca(U1, U2)
    }
  }
  return(d)
}