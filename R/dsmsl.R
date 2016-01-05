dsmsl <- function(x, delta, epsilon = min(x), log = FALSE){
    delta[ !is.finite(delta) | delta <= 0] <- NaN
    epsilon[ !is.finite(epsilon) | epsilon <= 0 | epsilon > 1/delta ] <- NaN
    c1 <- 1/(delta*epsilon - log(delta*epsilon) - 1)
    y <- c1 * (1/x - delta)
    y[ x < epsilon | x > 1/delta] <- 0
    if (any(is.nan(y))) warning ("NaNs produced")
    if(log) return(log(y))
    else return(y)
}
