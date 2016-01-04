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
# Some generators (such as continuous) are simply wrappers for [q]dist
# Some others need to 1-shift the result, using shift_r (on file utils.R)
# NOTE that we implement a new rpoilog for coherence with other generators

rbs<-function(n, N, S) qbs(runif(n), N, S)
rls <- function(n, N, alpha) qls(runif(n), N, alpha)
rpareto <- function(n, shape, scale = 1) qpareto(runif(n), shape, scale)
rpoig <- function(n, frac, rate, shape) qpoig(runif(n), frac, rate, shape)
rpoilog <- function(n, mu, sig) qpoilog(runif(n), mu, sig)
rpoix <- function(n, frac, rate) qpoix(runif(n), frac, rate)
rpower <- function(n, s) qpower(runif(n), s)

rgs <- function(n, k, S) shift_r("gs", n, list(k=k,S=S))
rmand <- function (n, N, s, v) shift_r("mand", n, list(N=N,s=s,v=v))
rmzsm <- function(n, J, theta) shift_r("mzsm", n, list(J=J, theta=theta))
rpowbend <- function(n, s, omega) shift_r("powbend", n, list(s=s, omega=omega))
rrbs <- function(n, N, S) shift_r("rbs", n, list(N=N,S=S))
rvolkov <- function(n, theta, m, J) shift_r("volkov", n, list(theta=theta,m=m,J=J))
rzipf <-function(n, N, s) shift_r("zipf", n, list(N=N, s=s))

## rtrunc for truncated versions of [r] functions
rtrunc <- function(f, n, trunc, coef)
  qtrunc(f, runif(n), trunc, coef)
