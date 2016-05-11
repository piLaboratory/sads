qpoig <- function(p, frac, rate, shape, lower.tail=TRUE, log.p=FALSE) {
    if(!lower.tail) p <- 1 - p
    op <- rank(p, ties.method="max")
    p <- sort(p)
    if(length(frac) > 1 | length(rate) > 1 | length(shape) > 1) stop("Vectorization not implemented for the parameters")
    if (log.p) p <- exp(p)
    y <- c()
    y[1] <- suppressWarnings(qfinder(dpoig, p[1], list(frac=frac, rate=rate, shape=shape), 0))
    if(length(p) > 1)
        for (i in 2:length(p))
            y[i] <- suppressWarnings(qfinder(dpoig, p[i], list(frac=frac, rate=rate, shape=shape), y[i-1]))
    if(any(is.nan(y))) warning("NaNs produced")
    return(y[op])
}
