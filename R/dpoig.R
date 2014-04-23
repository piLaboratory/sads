dpoig <- function(x, frac, rate, shape, log=FALSE) {
    is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)
        abs(x - round(x)) < tol
    if(FALSE %in% sapply(x,is.wholenumber))
        stop("x must be integer because dpoig is a discrete PDF.")
    else {
        f <- function(y) {
            b <- y*log(frac)+shape*log(rate)+lgamma(y+shape)
            c <- lfactorial(y)+lgamma(shape)+(y+shape)*log(frac+rate)
            exp(b-c)
          }
        vals <-f(x)
        if(log)log(vals) else vals
      }
}
