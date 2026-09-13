fitvolkov <- function(x, trunc, trunc.max, start.value, ...){
  dots <- list(...)
  if (any(x <= 0) | any(!is.wholenumber(x))) stop ("All x must be positive integers")
  if(missing(start.value)){
    tmp <- tempfile()
	  sink(tmp) # as the following function outputs lots of garbage...
	  start.value <- maxLikelihood.ESF(c(5, 0.5), x)$par
	  sink()
    file.remove(tmp) ## as sink("dev/null") does not work in al OS'
  }
  thetahat <- start.value[1]
  mhat <-start.value[2]
  if(!"method" %in% names(dots)){
    dots$method <- "L-BFGS-B"
    if(!"lower" %in% names(dots)) dots$lower=c(theta=thetahat/5, m=1e-4)
    if(!"upper" %in% names(dots)) dots$upper=c(theta=thetahat*5, m=0.9999)
  }
  if (!missing(trunc)){
    if (min(x)<=trunc) stop("truncation point should be lower than the lowest data value")
  }
  if (!missing(trunc.max)){
    if (max(x)>trunc.max) stop("trunc.max should not be lower than the highest data value")
  }
  trunc.args <- list()
  if (!missing(trunc)) trunc.args$trunc <- trunc
  if (!missing(trunc.max)) trunc.args$trunc.max <- trunc.max
  if (length(trunc.args) > 0){
    LL <- function(theta, m, J) {
      -sum(do.call(dtrunc, c(list("volkov", x = x,
                  coef = list(J = J, m = m, theta = theta), log = TRUE), trunc.args)))
    }
  } else {
    LL <- function(theta, m, J) -sum(dvolkov(x,  theta = theta, m = m, J = J, log = TRUE))
  }
  result <- do.call("mle2", c(list(minuslogl=LL, start = list(theta = thetahat, m = mhat), fixed=list(J=sum(x)), data = list(x = x)), dots))  
  new("fitsad", result, sad="volkov", distr = distr.depr, trunc = ifelse(missing(trunc), NaN, trunc), trunc.max = ifelse(missing(trunc.max), NaN, trunc.max))
}
