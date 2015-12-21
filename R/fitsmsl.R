fitsmsl <- function(x, trunc = NaN, start.value, ...){
  dots <- list(...)
  if (!is.nan(trunc)){
    warning("trunc argument for dsmsl is meaningless. Read the manual for details")
  }
  if (missing(start.value)){
    deltahat <- 0.5/max(x)
  } else{
    deltahat <- start.value[1]
  }
  if(!"method" %in% names(dots)){
    dots$method <- "Brent"
    if(!"lower" %in% names(dots)) dots$lower=list(delta=1e-16)
    if(!"upper" %in% names(dots)) dots$upper=list(delta=(1-1e-14)/max(x))
  }
  LL <- function(delta, epsilon) -sum(dsmsl(x, delta=delta, epsilon=epsilon, log = TRUE))
  result <- do.call("mle2", c(list(LL, start = list(delta = deltahat), fixed = list(epsilon = min(x)), data = list(x = x)), dots))
  new("fitsad", result, sad = "smsl", distr = distr.depr, trunc = NaN)
}

