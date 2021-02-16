pls <- function(q, N, alpha, lower.tail=TRUE, log.p=FALSE, use.ibeta = FALSE) {
    if(length(N) > 1 | length(alpha) > 1) stop("Vectorization not implemented for the parameters")
    nonwhole <- !is.wholenumber(q)
    if(use.ibeta){
        p <- N / (N + alpha)
        y <- 1 + ibeta(p, q+1, 1e-100) / log( 1 - p)
        if(!lower.tail)
            y <- 1-y
        if(log.p)
            y <- log(y)
    }
    if(!use.ibeta)
        y <- suppressWarnings(cumsumW(dls, q, list(N=N, alpha=alpha), lower.tail, log.p, pad=TRUE))        
    if (any(nonwhole)) {
        warning("non integer values in q")
        if(log.p) y[nonwhole] <- -Inf
        else y[nonwhole] <- 0  
    }
    if(any(is.nan(y))) warning("NaNs produced")
    return(y)
}
    
