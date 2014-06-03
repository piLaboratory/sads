ppsad <- function (object, sad, coef, trunc=NA, plot=TRUE, line=TRUE, ...) {
    if(class(object)=="fitsad"){
        sad <- object@sad
        coef <- as.list(bbmle::coef(object))
        trunc <- object@trunc
        x <- object@data$x
    }
    else if(class(object)=="numeric"|class(object)=="integer")
        x <- object
    x.sorted <- sort(x)
    S <- length(x)
    z <- ppoints(S)
    if(!is.na(trunc)){
        if(sad == "ls")
            p <- do.call(ptrunc, list(sad, q = x.sorted, coef = c(list(N=(sum(x)),coef)), trunc = trunc))
        else if(sad == "volkov"||sad=="mzsm")
            p <- do.call(ptrunc, list(sad, q = x.sorted, coef = c(list(J=(sum(x)),coef)), trunc = trunc))
        else
            p <- do.call(ptrunc, list(sad, q = x.sorted, coef = coef, trunc = trunc))
    }
    else{
        psad <- get(paste("p", sad, sep=""), mode = "function")
        if(sad == "ls")
            p <- do.call(psad, c(list(q = x.sorted, N=sum(x)), coef))
        else if(sad == "volkov"||sad=="mzsm")
            p <- do.call(psad, c(list(q = x.sorted, J=sum(x)), coef))
        else{
            p <- do.call(psad, c(list(q = x.sorted), coef))
        }
    }
    if(plot){
        dots <- list(...)
        if(!"main" %in% names(dots)) dots$main = "P-P plot"
        if(!"xlab" %in% names(dots)) dots$xlab = "Theoretical Percentiles"
        if(!"ylab" %in% names(dots)) dots$ylab = "Sample Percentiles"
        do.call(graphics::plot, c(list(x=p, y=z, ylim=c(0,1)),dots) )
        if(line) abline(0, 1, col = "red", lty = 2)
    }
    return(invisible(data.frame(theoret.p=p, sample.p=z)))
}
