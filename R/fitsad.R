fitsad <- function(x, sad=c("bs","gamma","geom","lnorm","ls","mzsm","nbinom","pareto","poilog","power","weibull","volkov", "poig", "poix"), ...){ 
  dots <- list(...)
  fit <- get(paste("fit", sad, sep=""), mode = "function")
  if(!"trunc" %in% names(dots) && (sad %in% c("poilog", "geom", "nbinom", "poig", "poix")))
    do.call(fit, c(list(x = x, trunc = 0), dots))
  else
    do.call(fit, c(list(x = x), dots))
}
