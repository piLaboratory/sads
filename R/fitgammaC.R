fitgammaC <- function(x, trunc, start.value, ...){
  dots <- list(...)
  if (any(x$breaks < 0)) stop ("All x must be positive")
  if(missing(start.value)){
      if(missing(trunc)){
          y <- rep(x$mids, x$counts)
          ka <- (mean(y)/sd(y))^2
          theta <- var(y)/mean(y)
          kahat <- function(k, dados){
              eq <- length(dados)*(log(k) - log(mean(dados)) - digamma(k)) + sum(log(dados))
          }
          ka <- uniroot(kahat, interval = c(min(theta, ka)/10, max(theta, ka)*10), dados = y)$root
          theta <- mean(y)/ka
      }
      else{
          xbr <- x$breaks
          Eh <- matrix(ncol=2, nrow=length(xbr)-1)
          for(i in 1:(length(xbr)-1)){
              m1 <- matrix(c(1,1,-1,1), ncol=2)
              Eh[i,] <- solve(m1, xbr[i:(i+1)])
          }
          P <- x$counts/sum(x$counts)
          P[P==0] <- min(P[P>0])
          Y <- log(P[-length(P)])-log(P[-1]) -(log(Eh[-nrow(Eh),2])-log(Eh[-1,2]))
          X1 <- Eh[-1,1]-Eh[-nrow(Eh),1]
          X2 <- log(Eh[-nrow(Eh),1])-log(Eh[-1,1])
          st1 <- unname(coef(lm(Y~X1+X2-1)))
          ka <- st1[2]+1
          theta <- 1/st1[1]
      }
  }
  else{
      ka <- start.value[1]
      theta <-start.value[2]
  }
  if(missing(trunc))
      LL <- function(shape, rate) -trueLL(x, dist = "gamma", coef = list(shape = shape, rate = rate))
  else
      LL <- function(shape, rate) -trueLL(x, dist = "gamma",
                                          coef = list(shape = shape, rate = rate), trunc = trunc)  
  result <- do.call("mle2", c(list(LL, start = list(shape = ka, rate = 1/theta)), dots))
  new("fitsadC", result, sad="gamma", trunc = ifelse(missing(trunc), NaN, trunc), hist = x)
}
