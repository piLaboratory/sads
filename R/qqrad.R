qqrad <- function(object, rad , coef, trunc=NA, distr, plot=TRUE){
  if(class(object)=="fitrad"){
    rad <- object@rad
    coef <- as.list(bbmle::coef(object))
    trunc <- object@trunc
    distr <- object@distr
    rad.tab <- object@rad.tab
  }
  else if(class(object)=="rad")
    rad.tab <- object
  else if(class(object)=="numeric")
    rad.tab <- rad(object)
  ranks <- rep(rad.tab$rank,rad.tab$abund)
  if(distr == "D"){
    q <- rad.tab$rank
    if(!is.na(trunc)){
      p <- do.call(ptrunc, list(rad, q = q, coef = coef, trunc = trunc))
    }
    else{
      prad <- get(paste("p", rad, sep=""), mode = "function")
      p <- do.call(prad, c(list(q = q), coef))
    }
    f1 <- approxfun(x=c(1, p), y=c(0, q), method="constant")
    q <- f1(ppoints(ranks))
  }
  else if(distr == "C"){
    p <- ppoints(ranks)
    if(!is.na(trunc))
      q <- do.call(qtrunc, list(rad, p = p, coef = coef, trunc = trunc))
    else{
      qrad <- get(paste("q", rad, sep=""), mode = "function")
      q <- do.call(qrad, c(list(p = p), coef))
    }
  }
  else
    stop("please choose 'D'iscrete or 'Continuous' for 'distr'")
  if(plot){
    plot(q, ranks, main = "Q-Q plot", xlab="Theoretical Quantile", ylab="Sample Quantiles")
    abline(0, 1, col = "red", lty = 2)
  }
  return(invisible(data.frame(theoret.q=q, sample.q=ranks)))
}
