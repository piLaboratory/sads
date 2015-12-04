rsad <- function(S, frac, sad, trunc=NaN, Pois.samp=TRUE, k, zeroes=FALSE, ssize=1, ...){
  dots <- list(...)
  if (!Pois.samp & missing(k)) stop("For negative binomial sampling please provide a value for k")
  if (ssize<1) stop("ssize must be at least one")
  if(is.nan(trunc)) {
    sad <- get (paste("r", sad, sep=""), mode = "function")
    com <- do.call(sad,c(list(n=S),dots))
  } else {
    com <- rtrunc(sad, n=S, trunc=trunc, coef=dots)
  }

  if(Pois.samp) sam=rpois(S*ssize,lambda=frac*com)
  else sam <- rnbinom(S*ssize,mu=frac*com,size=k)

  if(ssize>1){
    y <- data.frame(sample=rep(1:ssize,each=S), species=rep(1:S,ssize), abundance=sam)
    if(!zeroes) y <- y[y$abundance>0,]
  }
  else {
    y <- sam
    if(!zeroes) y <- y[y>0]
  }
  return(y)
}

### Random number generation from the sads implemented in this package
rbs<-function(n, N, S) qbs(runif(n), N, S)
rgs <- function(n, k, S) qgs(runif(n), k, S)
rls <- function(n, N, alpha) qls(runif(n), N, alpha)
rmand <- function (n, N, s, v) qmand(runif(n), N, s, v)
rmzsm <- function(n, J, theta) qmzsm(runif(n), J, theta)
rpareto <- function(n, shape, scale = 1) qpareto(runif(n), shape, scale)
rpoig <- function(n, frac, rate, shape) qpoig(runif(n), frac, rate, shape)
rpoilog <- function(n, mu, sig) qpoilog(runif(n), mu, sig)
rpoix <- function(n, frac, rate) qpoix(runif(n), frac, rate)
rpower <- function(n, s) qpower(runif(n), s)
rpowbend <- function(n, s, omega) qpower(runif(n), s, omega)
rrbs <- function(n, N, S) qrbs(runif(n), N, S)
rvolkov <- function(n, theta, m, J) qvolkov(runif(n), theta, m, J)
rzipf <- function(n, N, s) qzipf(runif(n), N, s)

## rtrunc for truncated versions of [r] functions
rtrunc <- function(f, n, trunc, coef)
  qtrunc(f, runif(n), trunc, coef)
