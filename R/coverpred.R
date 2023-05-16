coverpred <- function(object, ...){
    dots <- list(...)
    trunc <- object@trunc
    coef  <-  as.list(coef(object))
    q <- object@hist$breaks
    S <- sum(object@hist$counts)
    sad <- object@sad
    if(!is.nan(trunc)){
        Y <- do.call(ptrunc, c(list(sad, q = q, coef = coef, trunc = trunc), dots))
    }
    else{
        psad <- get(paste("p",sad,sep=""),mode="function")
        Y <- do.call(psad, c(list(q = q),coef, dots))
    }
    diff(Y)*S
}
