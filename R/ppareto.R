ppareto <- function(q, shape, scale = min(q), lower.tail = TRUE, log.p = FALSE){
  if(shape <= 0 || scale <= 0)
    stop("shape and scale must be greater than zero")
  y <- 1 - (scale/q)^shape
  y[q<scale] <- 0
  if (!lower.tail) 
    y <- 1 - y
  if (log.p) 
    y <- log(y)
  return(y)
}
