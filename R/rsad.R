rsad <- function(S = NULL, frac, 
                 sad = c("bs","gamma","geom","lnorm","ls","mzsm","nbinom","pareto",
                         "poilog","power", "powbend", "volkov", "weibull"), 
                 coef, trunc=NaN, sampling=c("poisson", "nbinom", "hypergeometric"), 
                 k, zeroes=FALSE, ssize=1) {
  sampling <- match.arg(sampling)
  sad <- match.arg(sad)
  if (frac <= 0 | frac > 1) stop("Invalid value for frac, make sure 0 < frac <= 1")
  if (sampling == "nbinom" & missing(k)) stop("For negative binomial sampling please provide a value for k")
  if (ssize<1) stop("ssize must be at least one")
  if (class(coef) != "list" | is.null(names(coef))) stop("coef must be a named list!")

  # Handles parameters that give the community size
  if (sad %in% c("bs", "ls", "mzsm", "volkov")) {
    if (!is.null(S))
      warning("For the selected sad the value of S is ignored")
    S <- switch(sad, 
                bs = coef$S,
                ls = coef$alpha * log ( 1 + coef$N / coef$alpha ),
                mzsm = sum(coef$theta / (1:coef$J) *(1 - (1:coef$J)/coef$J)^(coef$theta - 1)),
                volkov = suppressWarnings(Svolkov(coef$theta, coef$m, coef$J))
                )
  } else {
    if (is.null(S))
      stop("The argument S is mandatory for the selected sad")
  }

  # Generates the "community"
  if(is.nan(trunc)) {
    sadr <- get(paste("r", sad, sep=""), mode = "function")
    com <- do.call(sadr,c(list(n=S),coef))
  } else {
    com <- rtrunc(sad, n=S, trunc=trunc, coef=coef)
  }

  # Generates a sample from the community
  sam <- switch(sampling,
                poisson = rpois(S*ssize,lambda=frac*com),
                nbinom = rnbinom(S*ssize,mu=frac*com,size=k),
                hypergeometric = rfixed(com, frac, ssize)
               )

  # Treats "ssize" and "zeroes" arguments
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


rfixed <- function(com, frac, ssize) {
    rr <- rep(1:length(com), com)
    sam <- c()
    for (i in 1:ssize) {
        ss <- sample(rr, size = frac * length(rr), replace=FALSE)
        sam <- c(sam, hist(ss, breaks=0:max(ss), plot=F)$counts)
    }
    return(sam)
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

