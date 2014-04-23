fitpareto <- function(x, trunc, start.value, trueLL = TRUE, dec.places = 0, upper = 20, ...){
  dots <- list(...)
  if (!missing(trunc)){
    if (min(x)<=trunc) stop("truncation point should be lower than the lowest data value")
  }
  if(missing(start.value)){
    alpha <- length(x)/sum(log(x)-log(min(x)))
  } else{
    alpha <- start.value[1]
  }
  if (missing(trunc)){
    LL <- function(shape,scale) -sum(dpareto(x, shape, scale, log = TRUE))
  } else {
    LL <- function(shape, scale) -sum(dtrunc("pareto", x = x,
                                             coef = list(shape = shape, scale = scale), trunc = trunc, log = TRUE))
  }  
  result <- mle2(LL, start = list(shape = alpha, scale=min(x)), fixed=list(scale=min(x)),
                 data = list(x = x), method = "Brent", lower = 0, upper = upper, ...)
  if(abs(as.numeric(result@coef) - upper) < 0.001) 
    warning("mle equal to upper bound provided. \n Try value for the 'upper' argument")
  if(trueLL){
    warning("trueLL used, \n check if the precision in your data matches the dec.places argument \n")
    result@min <- -trueLL(x = x, dens = "pareto", coef = as.list(coef(result)), trunc, dec.places = dec.places, log = TRUE, ...)
  }
  new("fitsad", result, sad="pareto", distr = "C", trunc = ifelse(missing(trunc), NaN, trunc)) 
}
