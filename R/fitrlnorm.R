#drlnorm <- function(x, meanlog, sdlog, S, log=FALSE) {
#   qlnorm(ppoints(S)[x], meanlog, sdlog, lower.tail=FALSE, log=log) / sum(qlnorm(ppoints(S), meanlog, sdlog))
#}
#
#coef(fitlnorm(moths))->fit
#rad.tab <- rad(okland)
#y <- rep(rad.tab$rank, rad.tab$abund)
#
#fitmand(okland)
#
#x <- -sum(log(drlnorm(y, f[1], f[2], length(moths), FALSE)))
#
drad <- function(f, x, log = FALSE, S = length(x), ...) {
  dots <- list(...)
  rad.tab <- rad(x)
  y <- rep(rad.tab$rank, rad.tab$abund)
  p <- ppoints(S)
  qf <- get(paste("q", f, sep=""), mode = "function")
  num <- do.call(qf, c(list(p=p[y], log=log, lower.tail=FALSE), dots))
  den <- do.call(qf, c(list(p=p, log=FALSE), dots))
  if (log) 
    num - log(sum(den))
  else 
    num / sum(den)
}
#
#sum(log(drad("lnorm", moths, meanlog=fit[1], sdlog=fit[2], log=FALSE)))
#logLik(fitzipf(moths))
#
#dots<-list(meanlog=fit[1], sdlog=fit[2])
#f="lnorm"
