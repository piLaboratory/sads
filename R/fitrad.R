fitrad <- function(x, rad =c("gs", "mand", "rbs", "zipf"), ...){ 
  dots <- list(...)
  fit <- get(paste("fit", rad, sep=""), mode = "function")
  do.call(fit, c(list(x = x), dots))
}
