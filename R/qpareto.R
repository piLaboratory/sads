qpareto <- function (p, shape, scale = min(q), lower.tail = TRUE, log.p = FALSE) {
  if(shape <= 0 || scale <= 0)
    stop("shape and scale must be greater than zero")
  if (log.p) 
    p <- exp(p)
  if (!lower.tail) 
    p <- 1 - p
  d <- scale/(1 - p)^(1/shape)
  return(d)
}