fitexp <- function(x, trunc = NULL, start.value, ...){
	dots <- list(...)
  if ((any(x < 0) & !is.null(trunc))) stop ("No x should be negative")
  if (!is.null(trunc)){
    if (min(x)<=trunc) stop("truncation point should be lower than the lowest data value")
  }
  if (missing(start.value)){
    phat <- 1/(mean(x))
  }
  else{
    phat <- start.value
  }
  if(!"method" %in% names(dots)){
    dots$method <- "Brent"
    if(!"lower" %in% names(dots)) dots$lower=max(c(phat/10, 1e-8))
    if(!"upper" %in% names(dots)) dots$upper=min(c(phat*10, 0.99))
  }
  if (is.null(trunc)){
    LL <- function(rate) -sum(dexp(x, rate, log = TRUE))
  } else{
    LL <- function(rate) -sum(dtrunc("exp", x = x, coef = rate, trunc = trunc, log = TRUE))
  }
  result <- do.call("mle2", c(list(LL, start = list(rate = phat), data = list(x = x)), dots))  
  new("fitsad", result, sad = "exp", distr = distr.depr, trunc = ifelse(is.null(trunc), NaN, trunc))
}
