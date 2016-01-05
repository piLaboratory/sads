psmsl <- function(q, delta, epsilon, lower.tail=TRUE, log.p=FALSE){
    delta[ !is.finite(delta) | delta <= 0] <- NaN
    epsilon[ !is.finite(epsilon) | epsilon <= 0 | epsilon > 1/delta ] <- NaN
    c1 <- 1/(delta*epsilon - log(delta*epsilon) - 1)
    y <- c1 * (log(q) - delta*q - log(epsilon) + delta*epsilon)
    y[ q < epsilon ] <- 0
    y[ q > 1/delta ] <- 1
    if (any(is.nan(y))) warning ("NaNs produced")
    if (!lower.tail) y <- 1 - y
    if(log.p) return(log(y))
    else return(y)
}
