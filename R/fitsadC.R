fitsadC <- function(x, sad=c("exp","gamma","lnorm","pareto", "weibull"), ...){ 
  dots <- list(...)
  sad <- match.arg(sad)
  fit <- get(paste("fit", sad, "C", sep=""), mode = "function")
  do.call(fit, c(list(x = x), dots))
}
