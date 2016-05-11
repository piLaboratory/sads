qpoix <- function(p, frac, rate, lower.tail=TRUE, log.p=FALSE) {
    if(!lower.tail) p <- 1 - p
    op <- rank(p, ties.method="max")
    p <- sort(p)
    if(length(frac) > 1 | length(rate) > 1) stop("Vectorization not implemented for the parameters")
    if (log.p) p <- exp(p)
    y <- c()
    y[1] <- suppressWarnings(qfinder(dpoix, p[1], list(frac=frac, rate=rate), 0))
    if(length(p) > 1)
        for (i in 2:length(p))
            y[i] <- suppressWarnings(qfinder(dpoix, p[i], list(frac=frac, rate=rate), y[i-1]))
    if(any(is.nan(y))) warning("NaNs produced")
    return(y[op])
}
