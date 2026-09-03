fitgeom <- function(x, trunc = 0, trunc.max = NULL, start.value, ...){
	dots <- list(...)
  if ((any(x <= 0) & !is.null(trunc)) | any(!is.wholenumber(x))) stop ("All x must be positive integers")
  if (!is.null(trunc)){
    if (min(x)<=trunc) stop("truncation point should be lower than the lowest data value")
  }
  if (!is.null(trunc.max)){
    if (max(x)>trunc.max) stop("trunc.max should not be lower than the highest data value")
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
  trunc.args <- list()
  if (!is.null(trunc)) trunc.args$trunc <- trunc
  if (!is.null(trunc.max)) trunc.args$trunc.max <- trunc.max
  if (length(trunc.args) > 0){
    LL <- function(prob) -sum(do.call(dtrunc, c(list("geom", x = x, coef = prob, log = TRUE), trunc.args)))
  } else{
    LL <- function(prob) -sum(dgeom(x, prob, log = TRUE))
  }
  result <- do.call("mle2", c(list(LL, start = list(prob = phat), data = list(x = x)), dots))  
  new("fitsad", result, sad = "geom", distr = distr.depr, trunc = ifelse(is.null(trunc), NaN, trunc), trunc.max = ifelse(is.null(trunc.max), NaN, trunc.max))
}
