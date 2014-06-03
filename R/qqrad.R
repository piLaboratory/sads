qqrad <- function(object, rad , coef, trunc=NA, plot=TRUE, line=TRUE, ...){
    if(class(object)=="fitrad"){
        rad <- object@rad
        coef <- as.list(bbmle::coef(object))
        trunc <- object@trunc
        rad.tab <- object@rad.tab
    }
    else if(class(object)=="rad")
        rad.tab <- object
    else if(class(object)=="numeric"|class(object)=="integer")
        rad.tab <- rad(object)
    pr <- cumsum(rad.tab$abund/sum(rad.tab$abund))
    if(!is.na(trunc))
        q <- do.call(qtrunc, list(rad, p = pr, coef = coef, trunc = trunc))
    else{
        qrad <- get(paste("q", rad, sep=""), mode = "function")
        q <- do.call(qrad, c(list(p = pr), coef))
    }
    if(plot){
        dots <- list(...)
        if(!"main" %in% names(dots)) dots$main = "Q-Q plot"
        if(!"xlab" %in% names(dots)) dots$xlab = "Theoretical Quantile"
        if(!"ylab" %in% names(dots)) dots$ylab = "Sample Quantiles"
        do.call(graphics::plot, c(list(x=q, y=rad.tab$rank),dots))
        if(line) abline(0, 1, col = "red", lty = 2)
    }
    return(invisible(data.frame(theoret.q=q, sample.q=rad.tab$rank)))
}
