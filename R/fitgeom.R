fitgeom <- function(x, trunc = 0, start.value, ...){
	dots <- list(...)
  if ((any(x <= 0) & !is.null(trunc)) | any(!is.wholenumber(x))) stop ("All x must be positive integers")
  if (!is.null(trunc)){
    if (min(x)<=trunc) stop("truncation point should be lower than the lowest data value")
  }
  if (missing(start.value)){
    phat <- 1/(mean(x))
  } else{
    phat <- start.value
  }
  if(!"method" %in% names(dots)){
    dots$method <- "Brent"
    if(!"lower" %in% names(dots)) dots$lower=max(c(phat/10, 1e-8))
    if(!"upper" %in% names(dots)) dots$upper=min(c(phat*10, 0.99))
  }
  if (is.null(trunc)){
    LL <- function(prob) -sum(dgeom(x, prob, log = TRUE))
  } else{
    LL <- function(prob) -sum(dtrunc("geom", x = x, coef = prob, trunc = trunc, log = TRUE))
  }
  result <- do.call("mle2", c(list(LL, start = list(prob = phat), data = list(x = x)), dots))  
  new("fitsad", result, sad = "geom", distr = distr.depr, trunc = ifelse(is.null(trunc), NaN, trunc))
}
