fitweibull <- function(x, trunc, start.value, ...){
  dots <- list(...)
  if (!missing(trunc)){
    if (min(x)<=trunc) stop("truncation point should be lower than the lowest data value")
  }
  if(missing(start.value)){
    ka <- 1
    theta <- mean(x)
    for(i in 1:100){
      theta <- (sum(x^ka)/length(x))^(1/ka)
      ka <- length(x)/(sum(x^ka * log(x)) - sum(log(x))/theta)
    }
  } else{
    ka <- start.value[1]
    theta <-start.value[2]
  }
  if (missing(trunc)){
    LL <- function(shape, scale) -sum(dweibull(x, shape, scale, log = TRUE))
  } else {
    LL <- function(shape, scale) -sum(dtrunc("weibull", x = x, coef = list(shape, scale), trunc = trunc, log = TRUE))
  }  
  result <- mle2(LL, start = list(shape = ka, scale = theta), data = list(x = x), ...)
  new("fitsad", result, sad="weibull", distr = "C", trunc = ifelse(missing(trunc), NaN, trunc)) 
}
