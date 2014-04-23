fitvolkov <- function(x, trunc, start.values, ...){
  dots <- list(...)
  if(missing(start.values)){
    thetahat <- optimal.theta(x)
    mhat <- 0.5
  }
  else{
    thetahat <- start.values[[1]]
    mhat <-start.values[[2]]
  }
  if(!"method" %in% names(dots)){
    dots$method <- "L-BFGS-B"
    if(!"lower" %in% names(dots)) dots$lower=c(theta=thetahat/5, m=1e-4)
    if(!"upper" %in% names(dots)) dots$upper=c(theta=thetahat*5, m=0.9999)
  }
  if (!missing(trunc)){
    if (min(x)<=trunc) stop("truncation point should be lower than the lowest data value")
  }
  if (missing(trunc)){
    LL <- function(theta, m) -sum(dvolkov(x,  theta = theta, m = m, J = sum(x), log = TRUE))
  }
  else {
    LL <- function(theta, m) {
      -sum(dtrunc("volkov", x = x,
                  coef = list(J = sum(x), m = m, theta = theta),
                  trunc = trunc, log = TRUE))
    }
  }
  result <- do.call("mle2", c(list(minuslogl=LL, start = list(theta = thetahat, m = mhat), data = list(x = x)), dots))  
  new("fitsad", result, sad="volkov", distr = "D", trunc = ifelse(missing(trunc), NaN, trunc))
}
