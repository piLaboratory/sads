qqrad <- function(object, rad , coef, trunc=NA, distr, plot=TRUE, line=TRUE, ...){
    if(class(object)=="fitrad"){
        rad <- object@rad
        coef <- as.list(bbmle::coef(object))
        trunc <- object@trunc
        distr <- object@distr
        rad.tab <- object@rad.tab
    }
    else if(class(object)=="rad")
        rad.tab <- object
    else if(class(object)=="numeric"|class(object)=="integer")
        rad.tab <- rad(object)
    ranks <- rep(rad.tab$rank,rad.tab$abund)
    if(distr == "D"){
        q1 <- rad.tab$rank
        if(!is.na(trunc)){
            p <- do.call(ptrunc, list(rad, q = q1, coef = coef, trunc = trunc))
        }
        else{
            prad <- get(paste("p", rad, sep=""), mode = "function")
            p <- do.call(prad, c(list(q = q1), coef))
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
        dots <- list(...)
        if(!"main" %in% names(dots)) dots$main = "Q-Q plot"
        if(!"xlab" %in% names(dots)) dots$xlab = "Theoretical Quantile"
        if(!"ylab" %in% names(dots)) dots$ylab = "Sample Quantiles"
        do.call(graphics::plot, c(list(x=q, y=ranks),dots))
        ## plot(q, ranks, main = "Q-Q plot", xlab="Theoretical Quantile", ylab="Sample Quantiles")
        if(line) abline(0, 1, col = "red", lty = 2)
    }
    return(invisible(data.frame(theoret.q=q, sample.q=ranks)))
}
