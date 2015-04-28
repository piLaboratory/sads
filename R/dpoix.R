dpoix <- function(x, frac, rate, log=FALSE) {
    is.wholenumber <- function(x, tol = .Machine$double.eps^0.5){
      abs(x - round(x)) < tol
    }
    if(sum(x,is.wholenumber(x))<length(x))
       stop("dpoix is a discrete PDF; all y's must be integers")
    else {
      f <- function(y){
        b <- y*log(frac)
        m <- log(rate)
        n <- (y+1)*log(rate+frac)
        exp(b+m-n)
      }
      samp <- f(x)
      if(!log)samp else log(samp)
    }
  }
       
    
