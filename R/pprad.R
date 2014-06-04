pprad <- function (object, rad, coef, trunc=NA, plot=TRUE, line=TRUE, ...) {
  if(class(object)=="fitrad"){
    rad <- object@rad
    coef <- as.list(bbmle::coef(object))
    trunc <- object@trunc
    rad.tab <- object@rad.tab
  }
  else if(class(object)=="numeric"|class(object)=="integer")
      rad.tab <- rad(object)
  pr <- cumsum(rad.tab$abund/sum(rad.tab$abund))
  if(!is.na(trunc)){
    p <- do.call(ptrunc, list(rad, q = rad.tab$rank, coef = coef, trunc = trunc))
  }
  else{
    prad <- get(paste("p", rad, sep=""), mode = "function")
    p <- do.call(prad, c(list(q = rad.tab$rank), coef))
  }
  if(plot){
      dots <- list(...)
      if(!"main" %in% names(dots)) dots$main = "P-P plot"
      if(!"xlab" %in% names(dots)) dots$xlab = "Theoretical Percentiles"
      if(!"ylab" %in% names(dots)) dots$ylab = "Sample Percentiles"
      do.call(graphics::plot, c(list(x=p, y=pr, ylim=c(0,1)),dots) )
      if(line) abline(0, 1, col = "red", lty = 2)
  }
  return(invisible(data.frame(theoret.p=p, sample.p=pr)))
}
