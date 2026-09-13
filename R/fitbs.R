fitbs <- function(x, trunc, trunc.max, ...){
    dots <-list(...)
    if (any(x <= 0)) stop ("All x must be positive")
    s <- length(x)
    n <- sum(x)
    if (!missing(x)){
        if (!missing(trunc)){
            if (min(x)<=trunc) stop("truncation point should be lower than the lowest data value")
        }
        if (!missing(trunc.max)){
            if (max(x)>trunc.max) stop("trunc.max should not be lower than the highest data value")
        }
        trunc.args <- list()
        if (!missing(trunc)) trunc.args$trunc <- trunc
        if (!missing(trunc.max)) trunc.args$trunc.max <- trunc.max
        if (length(trunc.args) > 0){
            LL <- function(N,S) -sum(do.call(dtrunc, c(list("bs", x = x, coef = list(N = N, S = S), log = TRUE), trunc.args)))
        } else {
            LL <- function(N,S) -sum(dbs(x = x, N = N, S = S, log = TRUE))
        }
        result <- do.call("mle2", c(list(minuslogl=LL, data = list(x = x), fixed=list(N=n, S=s), eval.only=TRUE), dots))
        new("fitsad", result, sad = "bs", distr = distr.depr, trunc = ifelse(missing(trunc), NaN, trunc), trunc.max = ifelse(missing(trunc.max), NaN, trunc.max))
    }
}
