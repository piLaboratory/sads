qpower <- function(p, s, lower.tail = TRUE, log.p = FALSE){
  if (s <= 1) stop("s must be greater than one")
  d <- NULL
  if (log.p) p <- exp(p)
  if(!lower.tail) p <- 1 - p
  busca <- function(U1, U2){
    repeat{
      tt <- ppower(U2, s)
      U2 <- ifelse(tt>=U1, U2-1, U2)
      a1 <- U2
      U2 <- ifelse(tt<U1, U2+1, U2)
      a2 <- U2
      if (ppower(min(a1, a2), s) < U1 & U1 <= ppower(max(a1, a2), s)){
        return(max(a1, a2))
      }
    }
  }
  for (i in 1:length(p)){
    U1 <- p[i]
    U2 <- round(runif(1, min=1, max=length(p)))
    if (U1 <= ppower(1, s)){
      d[i] <- 1
    } else if (U1 >= 0.999999999999999999){
      d[i] <- Inf
    } else {
      d[i] <- busca(U1, U2)
    }
  }
  return(d)
}