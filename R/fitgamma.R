fitgamma <- function(x, trunc, start.value, ...){
  dots <- list(...)
  if (!missing(trunc)){
    if (min(x)<=trunc) stop("truncation point should, be lower than the lowest data value")
  }
  if(missing(start.value)){
    ka <- (mean(x)/sd(x))^2
    theta <- var(x)/mean(x)
    kahat <- function(k, dados){
      eq <- length(dados)*(log(k) - log(mean(dados)) - digamma(k)) + sum(log(dados))
    }
    ka <- uniroot(kahat, interval = c(min(theta, ka), max(theta, ka)), dados = x)$root
    theta <- mean(x)/ka
  } else{
    ka <- start.value[1]
    theta <-start.value[2]
  }
  if (missing(trunc)){
    LL <- function(shape, rate) -sum(dgamma(x, shape, rate, log = TRUE))
  } else {
    LL <- function(shape, rate) -sum(dtrunc("gamma", x = x, coef = list(shape = shape, rate = rate), trunc = trunc, log = TRUE))
  }  
  result <- mle2(LL, start = list(shape = ka, rate = 1/theta), data = list(x = x), ...)
  new("fitsad", result, sad="gamma", distr = "C", trunc = ifelse(missing(trunc), NaN, trunc)) 
}
