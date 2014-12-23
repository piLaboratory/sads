## Modified ICtab function from bbmle to allow corrected likelihoods sensu Lindsey
function (..., type = c("AIC", "BIC", "AICc", "qAIC", "qAICc"), 
    weights = FALSE, delta = TRUE, base = FALSE, logLik = FALSE, 
    sort = TRUE, nobs = NULL, dispersion = 1, mnames, k = 2, which.trueLL) 
{
    L <- list(...)
    ## added to allow use of corrected likelihood sensu Lindsey
    if(!missing(which.trueLL)){
        if(any(which.trueLL>length(L))) stop ("at least one index in which.trueLL large than the number of models")
        LLs <- sapply(L[which.trueLL], trueLL)
        for(i in which.trueLL) LL[i]@details$value <- LL[[i]]@min <- LLs[i]
    }
    if (is.list(L[[1]]) && length(L) == 1) 
        L <- L[[1]]
    type <- match.arg(type)
    if (dispersion != 1) {
        if (type == "BIC") 
            stop("cannot specify dispersion with BIC")
        if (substr(type, 1, 1) != "q") {
            type = paste("q", type, sep = "")
            warning("dispersion!=1, type changed to ", type)
        }
    }
    if (type == "AICc" || type == "BIC" || type == "qAICc") {
        if (is.null(nobs)) {
            nobs <- sapply(L, nobs)
            if (length(unique(nobs)) > 1) 
                stop("nobs different: must have identical data for all objects")
            nobs <- nobs[1]
        }
    }
    ICs <- switch(type, AIC = sapply(L, AIC), BIC = sapply(L, 
        BIC), AICc = sapply(L, AICc, nobs = nobs), qAIC = sapply(L, 
        qAIC, dispersion = dispersion), qAICc = sapply(L, qAICc, 
        nobs = nobs, dispersion = dispersion))
    logLiks <- sapply(L, function(x) c(logLik(x)))
    if (is.matrix(ICs)) 
        ICs <- ICs["AIC", ]
    getdf <- function(x) {
        if (!is.null(df <- attr(x, "df"))) 
            return(df)
        else if (!is.null(df <- attr(logLik(x), "df"))) 
            return(df)
    }
    dIC <- ICs - min(ICs)
    dlogLiks <- logLiks - min(logLiks)
    df <- sapply(L, getdf)
    tab <- data.frame(df = df)
    if (delta) {
        dName <- paste0("d", type)
        tab <- cbind(setNames(data.frame(dIC), dName), tab)
        if (logLik) {
            tab <- cbind(data.frame(dLogLik = dlogLiks), tab)
        }
    }
    if (base) {
        tab <- cbind(setNames(data.frame(ICs), type), tab)
        if (logLik) {
            tab <- cbind(data.frame(logLik = logLiks), tab)
        }
    }
    if (!delta && !base) 
        stop("either 'base' or 'delta' must be TRUE")
    if (weights) {
        wts <- exp(-dIC/2)/sum(exp(-dIC/2))
        tab <- data.frame(tab, weight = wts)
    }
    if (missing(mnames)) {
        Call <- match.call()
        if (!is.null(names(Call))) {
            xargs <- which(names(Call) %in% names(formals())[-1])
        }
        else xargs <- numeric(0)
        mnames <- as.character(Call)[c(-1, -xargs)]
    }
    row.names(tab) <- mnames
    if (sort) {
        tab <- tab[order(ICs), ]
    }
    class(tab) <- "ICtab"
    tab
}
