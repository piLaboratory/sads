fitpowbend <- function(x, trunc, trunc.max, start.value, ...){
  dots <- list(...)
  if (any(x <= 0) | any(!is.wholenumber(x))) stop ("All x must be positive integers")
  if (!missing(trunc)){
    if (min(x)<=trunc) stop("truncation point should be lower than the lowest data value")
  }
  if (!missing(trunc.max)){
    if (max(x)>trunc.max) stop("trunc.max should not be lower than the highest data value")
  }
  if (missing(start.value)){
    gamahat <- function(ga, xvec) eq <- -sum(log(xvec)) - zeta(ga, deriv =1)*length(xvec)/zeta(ga)
    shat <- uniroot(gamahat, interval=c(1.01, 20), xvec = x)$root
	oMhat <- 3
  } else{
    shat <- start.value[1]
    oMhat <- start.value[2]
  }
  if(!"method" %in% names(dots)){
    dots$method <- "L-BFGS-B"
    if(!"lower" %in% names(dots)) dots$lower=c(s=0.1, oM=1)
    if(!"upper" %in% names(dots)) dots$upper=c(s=2.999, oM=16)
  }
  trunc.args <- list()
  if (!missing(trunc)) trunc.args$trunc <- trunc
  if (!missing(trunc.max)) trunc.args$trunc.max <- trunc.max
  if (length(trunc.args) > 0){
    LL <- function(s, oM) -sum(do.call(dtrunc, c(list("powbend", x = x, coef = list(s=s, oM=oM), log = TRUE), trunc.args)))
  } else{
    LL <- function(s, oM) -sum(dpowbend(x, s=s, oM =oM, log = TRUE))
  }
  result <- do.call("mle2", c(list(LL, start = list(s = shat, oM=oMhat), data = list(x = x)), dots))
  new("fitsad", result, sad = "powbend", distr = distr.depr, trunc = ifelse(missing(trunc), NaN, trunc), trunc.max = ifelse(missing(trunc.max), NaN, trunc.max))
}
