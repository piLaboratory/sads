qls <- function(p, N, alpha, lower.tail = TRUE, log.p = FALSE, fast = FALSE, lower = 0, upper = 2*N){
    if(length(N) > 1 | length(alpha) > 1) stop("Vectorization not implemented for the parameters")
    if(!lower.tail)
        p <- 1 - p
    if (log.p)
        p <- exp(p)
    if(fast){ 
        f2 <- function(target){
            f1 <- function(x){
                P <- N / (N + alpha)
                (1 + ibeta(P, x+1, 1e-100) / log( 1 - P)) - target
                ##pls(q = x, N = N, alpha = alpha, use.ibeta = TRUE ) - target
                }
                ceiling(uniroot(f1, lower = lower, upper = upper)$root)
        }
        y <- sapply(p, f2)
    }
    if(!fast){
        op <- rank(p, ties.method = "max")
        p <- sort(p)
        y <- c()
        y[1] <- suppressWarnings(qfinder(dls, p[1], list(N=N, alpha=alpha), 0))
        if(length(p) > 1)
            for (i in 2:length(p))
                y[i] <- suppressWarnings(qfinder(dls, p[i], list(N=N, alpha=alpha), y[i-1]))
        y <- y[op]
    }
    if(any(is.nan(y))) warning("NaNs produced")
    return(y)
}
