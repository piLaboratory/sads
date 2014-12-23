##setGeneric("points")

setMethod("plot", "rad",
          function(x, ...){
            dots <- list(...)
            if(!"log" %in% names(dots)) dots$log <- "y"
            if(!"xlab" %in% names(dots)) dots$xlab = "Species Rank"
            if(!"ylab" %in% names(dots)) dots$ylab = "Species Abundance"
            if(!"frame.plot" %in% names(dots)) dots$frame.plot = TRUE
            if(!"axes" %in% names(dots)){ 
              do.call(plot, c(list(x = x[, 1], y = x[, 2], axes=FALSE), dots))
              axis(2)
              sc <- axisTicks(range(x[, 1]),nint=10,log=FALSE)
              sc[sc==0] <- 1
              axis(1,at=sc)
            }
            if("axes" %in% names(dots)){ 
              do.call(plot, c(list(x = x[, 1], y = x[, 2]), dots))
            }
            
          }
            )

setMethod("points", "rad",
          function(x, ...){
            dots <- list(...)
            if(!"type" %in% names(dots)) dots$type = "p"
            if(!"col" %in% names(dots)) dots$col = "blue"
            do.call(points, c(list(x = x[, 1], y = x[, 2]), dots)) 
          }
          )

setMethod("lines", "rad",
          function(x, ...){
            dots <- list(...)
            if(!"type" %in% names(dots)) dots$type = "l"
            if(!"col" %in% names(dots)) dots$col = "blue"
            do.call(lines, c(list(x = x[, 1], y = x[, 2]), dots)) 
          }
)

setMethod("plot","octav",
          function(x, prop=FALSE, x.oct=FALSE, par.axis=list(), ...){
            dots <- list(...)
            x.hist <- rep(as.integer(as.character(x$octave)), as.integer(as.character(x$Freq)))
            h1 <- hist(x=x.hist,
                           breaks = c((min(as.integer(as.character(x$octave)))-1),as.integer(as.character(x$octave))),
                           plot=FALSE)
            if(prop) h1$counts <- h1$counts/sum(h1$counts)
            if(x.oct) xlab <- x[seq(1,length(x[,1]),2),1]
            if(!x.oct) xlab <- x[seq(1,length(x[,1]),2),2]
            if(!"col" %in% names(dots)) dots$col = "gray"
            if(!"main" %in% names(dots)) dots$main = ""
            if(!"ylab" %in% names(dots) & !prop) dots$ylab = "N of species"
            if(!"ylab" %in% names(dots) & prop) dots$ylab = "Proportion of species"
            if(!"xlab" %in% names(dots) & !x.oct) dots$xlab = "Abundance class"
            if(!"xlab" %in% names(dots) & x.oct) dots$xlab = "Abundance class (log2)"
            if(!"axes" %in% names(dots)){
                do.call(plot, c(list(x=h1, axes=FALSE),dots))
                n <- as.numeric(as.character(x[,1]))
                do.call(axis, c(list(side=2), par.axis))
                do.call(axis, c(list(side=1,at=n[seq(1,length(x[,1]),2)],
                     labels=xlab),par.axis))
            }
            else
              do.call(plot, c(list(x=h1,dots)))
          }
          )

setMethod("points","octav",
          function(x, prop=FALSE, ...){
            dots <- list(...)
            if(!"type" %in% names(dots)) dots$type="b"
            if(!"col" %in% names(dots)) dots$col="blue"
            X <- c((min(as.integer(as.character(x$octave)))-1), as.integer(as.character(x$octave)))
            X <- X[-length(X)]+diff(X)/2
            if(prop) Y <- x$Freq/sum(x$Freq)
            if(!prop) Y <- x$Freq
            do.call(points, c(list(x = X, y = Y), dots))
          }
          )

setMethod("lines","octav",
          function(x, prop=FALSE, ...){
            dots <- list(...)
            if(!"type" %in% names(dots)) dots$type="b"
            if(!"col" %in% names(dots)) dots$col="blue"
            X <- c((min(as.integer(as.character(x$octave)))-1), as.integer(as.character(x$octave)))
            X <- X[-length(X)]+diff(X)/2
            if(prop) Y <- x$Freq/sum(x$Freq)
            if(!prop) Y <- x$Freq
            do.call(lines, c(list(x = X, y = Y), dots))
          }
)

setMethod("plot","fitsad",
          function(x, which=1:4, ask = prod(par("mfcol")) < length(which) && dev.interactive(), ...){
            oct.df <- octav(x)
            rad.df <- rad(x)
            oct.pred <- octavpred(x)
            oct.ymax <- max(c(oct.df[, 3], oct.pred[, 3]), na.rm = TRUE)
            rad.pred <- radpred(x)
            rad.ylim <- range(c(rad.df[, 2], rad.pred[, 2]), na.rm = TRUE)
            if (ask) {
              oask <- devAskNewPage(TRUE)
              on.exit(devAskNewPage(oask))
            }
            if(1 %in% which){
              plot(oct.df, ylim = c(0, oct.ymax), ...)
              points(oct.pred, ...)
            }
            if(2 %in% which){
              plot(rad.df, ylim = rad.ylim, ...)
              lines(rad.pred, ...)
            }
            if(3 %in% which){
              qqsad(x, ...)
            }
            if(4 %in% which){
              ppsad(x, ...)
            }
          }
          )

setMethod("plot","fitrad",
          function(x, which=1:4, ask = prod(par("mfcol")) < length(which) && dev.interactive(), ...){
            oct.df <- octav(x)
            rad.df <- rad(x)
            oct.pred <- octavpred(x)
            oct.ymax <- max(c(oct.df[, 3], oct.pred[, 3]), na.rm = TRUE)
            rad.pred <- radpred(x)
            rad.ylim <- range(c(rad.df[, 2], rad.pred[, 2]), na.rm = TRUE)
            if (ask) {
              oask <- devAskNewPage(TRUE)
              on.exit(devAskNewPage(oask))
            }
            if(1 %in% which){
              plot(oct.df, ylim = c(0, oct.ymax), ...)
              points(oct.pred, ...)
            }
            if(2 %in% which){
              plot(rad.df, ylim = rad.ylim, ...)
              lines(rad.pred, ...)
            }
            if(3 %in% which){
              qqrad(x, ...)
            }
            if(4 %in% which){
              pprad(x, ...)
            }
          }
          )

## copy of the method in bbmle, with a line added to assure df is not NULL
setMethod("AICc","fitsad",
          function (object, ..., nobs, k = 2){
            L <- list(...)
            if (length(L)) {
              L <- c(list(object), L)
              if (missing(nobs) && is.null(attr(object, "nobs"))) 
                stop("must specify number of observations")
              nobs <- sapply(L, attr, "nobs")
              if (length(unique(nobs)) > 1) 
                stop("nobs different: must have identical data for all objects")
              logLiks <- sapply(L, logLik)
              df <- sapply(L, attr, "df")
              if(is.null(df)) df <- sapply(L, function(object) attr(logLik(object),"df")) ## added to assure that df is not NULL
              val <- -2 * logLiks + k * df * (df + 1)/(nobs - df - 1)
              data.frame(AICc = val, df = df)
            }
            else {
              df <- attr(object, "df") 
              if(is.null(df)) df <- attr(logLik(object),"df") ## added to assure that df is not NULL
              c(-2 * logLik(object) + k * df + k * df * (df + 1)/(nobs - 
                                                                  df - 1))
            }
          }
          )

## copy of the method in bbmle, with a line added to assure df is not NULL
setMethod("AICc","fitrad",
          function (object, ..., nobs, k = 2){
            L <- list(...)
            if (length(L)) {
              L <- c(list(object), L)
              if (missing(nobs) && is.null(attr(object, "nobs"))) 
                stop("must specify number of observations")
              nobs <- sapply(L, attr, "nobs")
              if (length(unique(nobs)) > 1) 
                stop("nobs different: must have identical data for all objects")
              logLiks <- sapply(L, logLik)
              df <- sapply(L, attr, "df")
              if(is.null(df)) df <- sapply(L, function(object) attr(logLik(object),"df")) ## added to assure that df is not NULL
              val <- -2 * logLiks + k * df * (df + 1)/(nobs - df - 1)
              data.frame(AICc = val, df = df)
            }
            else {
              df <- attr(object, "df") 
              if(is.null(df)) df <- attr(logLik(object),"df") ## added to assure that df is not NULL
              c(-2 * logLik(object) + k * df + k * df * (df + 1)/(nobs - 
                                                                  df - 1))
            }
          }
          )

## radpred generic functions and methods ###
setGeneric("radpred",
def = function(object, sad, rad, coef, trunc, distr, S, N, ...) standardGeneric("radpred")
           )

## if object is of class fitsad (no other argument should be provided)
setMethod("radpred",signature(object="fitsad", sad="missing", rad="missing",
                              coef="missing", trunc="missing", distr="missing", S="missing", N="missing"),
          function (object){
            coef <- as.list(bbmle::coef(object))
            trunc <- object@trunc
            distr <- object@distr
            sad <- object@sad
            x <- object@data$x
            N <- sum(x)
            S <- length(x)
            if (distr == "D"){
              y <- 1:N
              if(!is.na(trunc)){
                if(sad=="ls") 
                  X <- do.call(ptrunc, c(list(sad, q = y, coef = c(list(N = N),coef),
                                              lower.tail=F, trunc = trunc)))
                else if(sad=="mzsm"||sad=="volkov") 
                  X <- do.call(ptrunc, list(sad, q = y, coef = c(list(J = N), coef),
                                            lower.tail=F, trunc = trunc))
                else
                  X <- do.call(ptrunc, list(sad, q = y, coef = coef, lower.tail=F, trunc = trunc))
              }
              else {
                psad <- get(paste("p", sad, sep=""), mode = "function")
                if(sad=="ls")
                  X <- do.call(psad, c(list(q = y, lower.tail = F, N = N),coef))
                else if(sad=="mzsm"||sad=="volkov")
                  X <- do.call(psad, c(list(q = y, lower.tail = F, J = N), coef))
                else
                  X <- do.call(psad, c(list(q = y, lower.tail = F), coef))
              }
              f1 <- approxfun(x=c(1, X), y=c(0, y), method="constant")
              ab <- f1(ppoints(S))
            }
            else if(distr == "C"){
              Y <- ppoints(S)
              if(!is.na(trunc)){
                ab <- do.call(qtrunc, list(sad, p = Y, coef = coef, lower.tail=F, trunc = trunc))
              }
              else{
                qsad <- get(paste("q", sad, sep=""), mode = "function")
                ab <- do.call(qsad, c(list(p = Y, lower.tail = F), coef))
              }
            }
            new("rad", data.frame(rank=1:S, abund=ab))
          }
          )

## if object is of class fitrad (no other argument should be provided)
setMethod("radpred",signature(object="fitrad", sad="missing", rad="missing",
                              coef="missing", trunc="missing", distr="missing", S="missing", N="missing"),
          function(object, ...){
            dots <- list()
            x <- object@rad.tab$abund
            N <- sum(x)
            S <- length(x)
            y <- 1:S
            rad <- object@rad
            coef <- as.list(bbmle::coef(object))
            if(rad=="zipf"||rad=="mand") coef <- c(list(N=S),coef)
            if(rad=="gs") coef <- c(list(S=S),coef)
            trunc <- object@trunc
            distr <- object@distr
            if(!is.na(trunc)){
              ab <- do.call(dtrunc, c(list(rad, x = y, coef = coef, trunc = trunc), dots))*N
            }
            else{
              drad <- get(paste("d", rad, sep=""),  mode = "function")
              ab <- do.call(drad, c(list(x = y), coef, dots))*N
            }
            new("rad", data.frame(rank=1:S, abund=ab))
          }
          )

## if object is a numeric vector of abundances and rad argument is given (sad, S, N, distr,  arguments should be missing)
setMethod("radpred",signature(object="numeric", sad="missing", rad="character",
                              coef="list", distr="missing", S="missing", N="missing"),
          function(object, sad, rad, coef, trunc, ...){
            dots <- list(...)
            S <- length(object)
            N <- sum(object)
            y <- 1:S
            if(rad=="zipf"||rad=="mand") coef <- c(list(N=S),coef)
            if(rad=="gs") coef <- c(list(S=S),coef)
            if(!missing(trunc)){
              ab <- do.call(dtrunc, c(list(rad, x = y, coef = coef, trunc = trunc), dots))*N
            }
            else{
              drad <- get(paste("d", rad, sep=""),  mode = "function")
              ab <- do.call(drad, c(list(x = y), coef, dots))*N
            }
            new("rad", data.frame(rank=1:S, abund=ab))
          }
          )

## if object is a numeric vector of abundances and sad argument is given (rad, S, N,  arguments should be missing)
setMethod("radpred",signature(object="numeric", sad="character", rad="missing",
                              coef="list", distr="ANY", S="missing", N="missing"),
          function(object, sad, rad, coef, trunc, distr, ...){
              if( missing(distr) ) stop("Argument 'distr' missing, please choose \"D\" or \"C\"")
              dots <- list(...)
              S <- length(object)
              N <- sum(object)
              if (distr == "D"){
                  y <- 1:N
                  if(!missing(trunc)){
                      if(sad=="ls") 
                          X <- do.call(ptrunc, c(list(sad, q = y, coef = c(list(N = N),coef),
                                                      lower.tail=F, trunc = trunc)))
                      else if(sad=="mzsm"||sad=="volkov") 
                          X <- do.call(ptrunc, list(sad, q = y, coef = c(list(J = N), coef),
                                                    lower.tail=F, trunc = trunc))
                      else
                          X <- do.call(ptrunc, list(sad, q = y, coef = coef, lower.tail=F, trunc = trunc))
                  }
                  else {
                      psad <- get(paste("p", sad, sep=""), mode = "function")
                      if(sad=="ls")
                          X <- do.call(psad, c(list(q = y, lower.tail = F, N = N),coef))
                      else if(sad=="mzsm"||sad=="volkov")
                          X <- do.call(psad, c(list(q = y, lower.tail = F, J = N), coef))
                      else
                          X <- do.call(psad, c(list(q = y, lower.tail = F), coef))
                  }
                  f1 <- approxfun(x=c(1, X), y=c(0, y), method="constant")
                  ab <- f1(ppoints(S))
              }
              else if(distr == "C"){
                  Y <- ppoints(S)
                  if(!missing(trunc)){
                      ab <- do.call(qtrunc, list(sad, p = Y, coef = coef, lower.tail=F, trunc = trunc))
                  }
                  else{
                      qsad <- get(paste("q", sad, sep=""), mode = "function")
                      ab <- do.call(qsad, c(list(p = Y, lower.tail = F), coef))
                  }
              }
              new("rad", data.frame(rank=1:S, abund=ab))
          }
          )

## if object is missing and rad is given. sad should not be given. All other arguments except distr should be given,
## except trunc (optional)
setMethod("radpred", signature(object="missing", sad="missing", rad="character",
                              coef="list", distr="missing", S="numeric", N="numeric"),
          function(object, sad, rad, coef, trunc, distr, S, N, ...){
            dots <- list(...)
            y <- 1:S
            if(rad=="zipf"||rad=="mand") coef <- c(list(N=S),coef)
            if(rad=="gs") coef <- c(list(S=S),coef)
            if(!missing(trunc)){
              ab <- do.call(dtrunc, c(list(rad, x = y, coef = coef, trunc = trunc), dots))*N
            }
            else{
              drad <- get(paste("d", rad, sep=""),  mode = "function")
              ab <- do.call(drad, c(list(x = y), coef, dots))*N
            }
            new("rad", data.frame(rank=1:S, abund=ab))
          }
          )

## if object is missing and sad is given. rad should not be given.
## All other arguments except distr should be given, except trunc (optional)
setMethod("radpred", signature(object="missing", sad="character", rad="missing",
                              coef="list", distr="character", S="numeric", N="numeric"),
          function(object, sad, rad, coef, trunc, distr, S, N, ...){
            if (distr == "D"){
              y <- 1:N
              if(!missing(trunc)){
                if(sad=="ls") 
                  X <- do.call(ptrunc, c(list(sad, q = y, coef = c(list(N = N),coef),
                                              lower.tail=F, trunc = trunc)))
                else if(sad=="mzsm"||sad=="volkov") 
                  X <- do.call(ptrunc, list(sad, q = y, coef = c(list(J = N), coef),
                                            lower.tail=F, trunc = trunc))
                else
                  X <- do.call(ptrunc, list(sad, q = y, coef = coef, lower.tail=F, trunc = trunc))
              }
              else {
                psad <- get(paste("p", sad, sep=""), mode = "function")
                if(sad=="ls")
                  X <- do.call(psad, c(list(q = y, lower.tail = F, N = N),coef))
                else if(sad=="mzsm"||sad=="volkov")
                  X <- do.call(psad, c(list(q = y, lower.tail = F, J = N), coef))
                else
                  X <- do.call(psad, c(list(q = y, lower.tail = F), coef))
              }
              f1 <- approxfun(x=c(1, X), y=c(0, y), method="constant")
              ab <- f1(ppoints(S))
            }
            else if(distr == "C"){
              Y <- ppoints(S)
              if(!missing(trunc)){
                ab <- do.call(qtrunc, list(sad, p = Y, coef = coef, lower.tail=F, trunc = trunc))
              }
              else{
                qsad <- get(paste("q", sad, sep=""), mode = "function")
                ab <- do.call(qsad, c(list(p = Y, lower.tail = F), coef))
              }
            }
            new("rad", data.frame(rank=1:S, abund=ab))
          }
          )

          
## octavpred generic functions and methods ###
setGeneric("octavpred",
def = function(object, sad, rad, coef, trunc, oct, S, N, ...) standardGeneric("octavpred"))

## if object is of class fitsad (no other argument should be provided)
setMethod("octavpred", signature(object="fitsad",sad="missing", rad="missing",
                                 coef="missing", trunc="missing", oct="ANY",
                                 S="missing", N="missing"),
          function(object, sad, rad, coef, trunc, oct, S, N, ...){
            dots <- list(...)
            coef <- as.list(bbmle::coef(object))
            trunc <- object@trunc
            sad <- object@sad
            x <- object@data$x
            S <- length(x)
            N <- sum(x)
            if(missing(oct)){
                oct <- 0:(ceiling(max(log2(x)))+1)
                if(any(x < 1)){
                    octlower <- ceiling(min(log2((x)))):-1
                    oct <- c(octlower, oct)
                }
            }
            n <- 2^oct
            if(!is.na(trunc)){
              if(sad == "ls")
                Y <- do.call(ptrunc, c(list(f=sad, q = n, coef=c(list(N = N),coef),trunc = trunc),dots))
              else if(sad == "mzsm"||sad=="volkov")
                Y <- do.call(ptrunc, c(list(f=sad, q = n, coef = c(list(J = N),coef), trunc = trunc),dots))
              else
                Y <- do.call(ptrunc, c(list(sad, q = n, coef = coef, trunc = trunc), dots))
            }
            else{
              psad <- get(paste("p",sad,sep=""),mode="function")
              if(sad == "ls")
                Y <- do.call(psad, c(list(q = n, N = N),coef,dots))
              else if(sad == "mzsm" || sad == "volkov")
                Y <- do.call(psad, c(list(q = n, J = N),coef,dots))
              else
                Y <- do.call(psad, c(list(q = n),coef,dots))
            }
            Y <- c(Y[1], diff(Y))*S
            new("octav", data.frame(octave = oct, upper = factor(n), Freq = Y))
          }
          )

## if object is of class fitrad (no other argument should be provided, except oct (optional))
setMethod("octavpred", signature(object="fitrad",sad="missing", rad="missing",
                                 coef="missing", trunc="missing", oct="ANY",
                                 S="missing", N="missing"),
          function(object, sad, rad, coef, trunc, oct, S, N, ...){
            dots <- list(...)
            coef <- as.list(bbmle::coef(object))
            trunc <- object@trunc
            rad <- object@rad
            x <- object@rad.tab$abund
            S <- length(x)
            N <- sum(x)
            if(rad=="zipf"||rad=="mand") coef <- c(list(N=S),coef)
            if(rad=="gs") coef <- c(list(S=S),coef)
            if(missing(oct)){
                oct <- 0:(ceiling(max(log2(x)))+1)
                if(any(x < 1)){
                    octlower <- ceiling(min(log2((x)))):-1
                    oct <- c(octlower, oct)
                }
            }
            n <- 2^oct
            if(!is.na(trunc)){
              ab <- do.call(dtrunc, c(list(f=rad, q = 1:S, coef=coef,trunc = trunc),dots))*N
            }
            else{
              drad <- get(paste("d",rad,sep=""),mode="function")
              ab <- do.call(drad, c(list(x=1:S),coef,dots))*N
            }
            Y <- as.numeric(table(cut(ab,breaks=c(2^(min(oct)-2),n))))  
            new("octav", data.frame(octave = oct, upper = factor(n), Freq = Y))
          }
          )
          
## if object is a numeric vector of abundances and rad argument is given (sad, S, N,  arguments should be missing)
setMethod("octavpred", signature(object="numeric",sad="missing", rad="character",
                                 coef="list", trunc="ANY", oct="ANY", S="missing", N="missing"),
          function(object, sad, rad, coef, trunc, oct, S, N, ...){
            dots <- list(...)
            x <- object
            S <- length(x)
            N <- sum(x)
            if(rad=="zipf"||rad=="mand") coef <- c(list(N=S),coef)
            if(rad=="gs") coef <- c(list(S=S),coef)
            if(missing(oct)){
                oct <- 0:(ceiling(max(log2(x)))+1)
                if(any(x < 1)){
                    octlower <- ceiling(min(log2((x)))):-1
                    oct <- c(octlower, oct)
                }
            }
            n <- 2^oct
            if(!missing(trunc)){
              ab <- do.call(dtrunc, c(list(f=rad, q = 1:S, coef=coef,trunc = trunc),dots))*N
            }
            else{
              drad <- get(paste("d",rad,sep=""),mode="function")
              ab <- do.call(drad, c(list(x=1:S),coef,dots))*N
            }
            Y <- as.numeric(table(cut(ab,breaks=c(2^(min(oct)-2),n))))  
            new("octav", data.frame(octave = oct, upper = factor(n), Freq = Y))
          }
)

## if object is a numeric vector of abundances and sad argument is given (rad, S, N,  arguments should be missing)
setMethod("octavpred", signature(object="numeric",sad="character", rad="missing",
                                 coef="list", S="missing", N="missing"),
          function(object, sad, rad, coef, trunc, oct, S, N, ...){
            dots <- list(...)
            x <- object
            S <- length(x)
            N <- sum(x)
            if(missing(oct)){
                oct <- 0:(ceiling(max(log2(x)))+1)
                if(any(x < 1)){
                    octlower <- ceiling(min(log2((x)))):-1
                    oct <- c(octlower, oct)
                }
            }
            n <- 2^oct
            if(!missing(trunc)){
              if(sad == "ls")
                Y <- do.call(ptrunc, c(list(f=sad, q = n, coef=c(list(N = N),coef),trunc = trunc),dots))
              else if(sad == "mzsm"||sad=="volkov")
                Y <- do.call(ptrunc, c(list(f=sad, q = n, coef = c(list(J = N),coef), trunc = trunc),dots))
              else
                Y <- do.call(ptrunc, c(list(sad, q = n, coef = coef, trunc = trunc), dots))
            }
            else{
              psad <- get(paste("p",sad,sep=""),mode="function")
              if(sad == "ls")
                Y <- do.call(psad, c(list(q = n, N = N),coef,dots))
              else if(sad == "mzsm" || sad == "volkov")
                Y <- do.call(psad, c(list(q = n, J = N),coef,dots))
              else
                Y <- do.call(psad, c(list(q = n),coef,dots))
            }
            Y <- c(Y[1], diff(Y))*S
            new("octav", data.frame(octave = oct, upper = factor(n), Freq = Y))
          }
          )

## if object is missing and rad is given. sad should not be given.
## All other arguments except distr should be given, except trunc (optional)
setMethod("octavpred", signature(object="missing",sad="missing", rad="character",
                                 coef="list", S="numeric", N="numeric"),
          function(object, sad, rad, coef, trunc, oct, S, N, ...){
            dots <- list(...)
            if(rad=="zipf"||rad=="mand") coef <- c(list(N=S),coef)
            if(rad=="gs") coef <- c(list(S=S),coef)
            n <- 2^oct
            if(!missing(trunc)){
              ab <- do.call(dtrunc, c(list(f=rad, q = 1:S, coef=coef,trunc = trunc),dots))*N
            }
            else{
              drad <- get(paste("d",rad,sep=""),mode="function")
              ab <- do.call(drad, c(list(x=1:S),coef,dots))*N
            }
            Y <- as.numeric(table(cut(ab,breaks=c(2^(min(oct)-2),n))))  
            new("octav", data.frame(octave = oct, upper = factor(n), Freq = Y))
          }
)


## if object is missing and sad is given. rad should not be given.
## All other arguments except distr should be given, except trunc (optional)
setMethod("octavpred", signature(object="missing",sad="character", rad="missing",
                                 coef="list", S="numeric", N="numeric"),
          function(object, sad, rad, coef, trunc, oct, S, N, ...){
            dots <- list(...)
            n <- 2^oct
            if(!missing(trunc)){
              if(sad == "ls")
                Y <- do.call(ptrunc, c(list(f=sad, q = n, coef=c(list(N = N),coef),trunc = trunc),dots))
              else if(sad == "mzsm"||sad=="volkov")
                Y <- do.call(ptrunc, c(list(f=sad, q = n, coef = c(list(J = N),coef), trunc = trunc),dots))
              else
                Y <- do.call(ptrunc, c(list(sad, q = n, coef = coef, trunc = trunc), dots))
            }
            else{
              psad <- get(paste("p",sad,sep=""),mode="function")
              if(sad == "ls")
                Y <- do.call(psad, c(list(q = n, N = N),coef,dots))
              else if(sad == "mzsm" || sad == "volkov")
                Y <- do.call(psad, c(list(q = n, J = N),coef,dots))
              else
                Y <- do.call(psad, c(list(q = n),coef,dots))
            }
            Y <- c(Y[1], diff(Y))*S
            new("octav", data.frame(octave = oct, upper = factor(n), Freq = Y))
          }
          )

## Generic and methods for qqsad
setGeneric("qqsad",
def = function(x, sad, coef, trunc=NA, distr, plot=TRUE, line=TRUE, ...) standardGeneric("qqsad"))

## method for class numeric
## if x is numeric (abundances), all other arguments should be given.
## Only trunc, plot and line are optional because they have default values
setMethod("qqsad",
          signature(x="numeric", sad="character", coef="list", distr="character"),
          function(x, sad, coef, trunc=NA, distr, plot=TRUE, line=TRUE, ...){
              x.sorted <- sort(x)
              S <- length(x)
              if(distr == "D"){
                  q <- 1:sum(x)
                  if(!is.na(trunc)){
                      if(sad == "ls")
                          p <- do.call(ptrunc, list(sad, q = q, coef = c(list(N=(sum(x)),coef)), trunc = trunc))
                      else if(sad == "volkov"|| sad=="mzsm")
                          p <- do.call(ptrunc, list(sad, q = q, coef = c(list(J=(sum(x)),coef)), trunc = trunc))
                      else
                          p <- do.call(ptrunc, list(sad, q = q, coef=coef, trunc=trunc))
                  }
                  else{
                      psad <- get(paste("p", sad, sep=""), mode = "function")
                      if(sad == "ls")
                          p <- do.call(psad, c(list(q = q, N = sum(x)), coef))
                      else if(sad =="volkov"||sad=="mzsm")
                          p <- do.call(psad, c(list(q = q, J=sum(x)), coef))
                      else{
                          p <- do.call(psad, c(list(q = q), coef))
                      }
                  }
                  f1 <- approxfun(x=c(1, p), y=c(0, q), method="constant")
                  q <- f1(ppoints(S))
              }
              else if(distr == "C"){
                  p <- ppoints(S)
                  if(!is.na(trunc))
                      q <- do.call(qtrunc, list(sad, p = p, trunc = trunc, coef=coef))
                  else{
                      qsad <- get(paste("q", sad, sep=""), mode = "function")
                      q <- do.call(qsad, c(list(p = p), coef))
                  }
              }
              else
                  stop("please choose 'D'iscrete or 'C'ontinuous for 'distr'")
              if(plot){
                  dots <- list(...)
                  if(!"main" %in% names(dots)) dots$main = "Q-Q plot"
                  if(!"xlab" %in% names(dots)) dots$xlab = "Theoretical Quantile"
                  if(!"ylab" %in% names(dots)) dots$ylab = "Sample Quantiles"
                  do.call(graphics::plot, c(list(x=q, y=x.sorted),dots))
                  if(line) abline(0, 1, col = "red", lty = 2)
              }
              return(invisible(data.frame(theoret.q=q, sample.q=x.sorted)))
          }
          )

## For integer values
## setMethod("qqsad",
##           signature(x="integer", sad="character", coef="list", distr="character"),
##           function(x, sad, coef, trunc=NA, distr, plot=TRUE, line=TRUE, ...){
##               y <- as.numeric(x)
##               qqsad(x=y, sad=sad, coef=coef, trunc=trunc,
##                     distr=distr, plot=plot, line=line, ...)
##           }
##           )

## If x is of the class fitsad all other arguments should be ommited
## plot and line have default values and are optional
setMethod("qqsad",
          signature(x="fitsad", sad="missing", coef="missing",
                    trunc="missing", distr="missing"),
          function(x, sad, coef, trunc, distr, plot=TRUE, line=TRUE, ...){
              sad <- x@sad
              coef <- as.list(bbmle::coef(x))
              trunc <- x@trunc
              distr <- x@distr
              y <- x@data$x
              qqsad(x=y, sad=sad, coef=coef, trunc=trunc, distr=distr, plot=plot, line=line, ...)
          }
          )


## Generic and methods for qqrad
setGeneric("qqrad",
def = function(x, rad, coef, trunc=NA, plot=TRUE, line=TRUE, ...) standardGeneric("qqrad"))

## If x is an object of class rad
setMethod("qqrad",
          signature(x="rad", rad="character", coef="list"),
          function(x, rad , coef, trunc=NA, plot=TRUE, line=TRUE, ...){
              pr <- cumsum(x$abund/sum(x$abund))
              if(rad=="zipf"||rad=="mand") coef <- c(list(N=length(pr)),coef)
              if(rad=="gs") coef <- c(list(S=length(pr)),coef)
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
                  do.call(graphics::plot, c(list(x=q, y=x$rank),dots))
                  if(line) abline(0, 1, col = "red", lty = 2)
              }
              return(invisible(data.frame(theoret.q=q, sample.q=x$rank)))
          }
          )

## If object is of class numeric arguments rad and coef should be provided
setMethod("qqrad",
          signature(x="numeric", rad="character", coef="list"),
          function(x, rad , coef, trunc=NA, plot=TRUE, line=TRUE, ...){
              y <- rad(x)
              qqrad(x=y, rad=rad, coef=coef, trunc=trunc, plot=plot, line=line, ...)
          }
          )

## If object is of class integer arguments rad and coef should be provided
## setMethod("qqrad",
##           signature(x="integer", rad="character", coef="list",
##                     trunc="ANY", plot="ANY", line="ANY"),
##           function(x, rad , coef, trunc=NA, plot=TRUE, line=TRUE, ...){
##               y <- as.numeric(x)
##               qqrad(x=y, rad=rad, coef=coef, trunc=trunc, plot=plot, line=line, ...)
##           }
##           )

## If object is of class fitrad arguments rad or coef should be missing
setMethod("qqrad",
          signature(x="fitrad", rad="missing", coef="missing", trunc="missing"),
          function(x, rad , coef, trunc, plot=TRUE, line=TRUE, ...){
              rad <- x@rad
              coef <- as.list(bbmle::coef(x))
              trunc <- x@trunc
              y <- x@rad.tab
              qqrad(x=y, rad=rad, coef=coef, trunc=trunc, plot=plot, line=line, ...)
          }
          )


## Generic function and methods for ppsad ##
setGeneric("ppsad",
def = function(x, sad, coef, trunc=NA, plot=TRUE, line=TRUE, ...) standardGeneric("ppsad"))

## If x is numeric arguments sad and coef should be provided
setMethod("ppsad",
          signature(x="numeric", sad="character", coef="list"),
          function (x, sad, coef, trunc=NA, plot=TRUE, line=TRUE, ...) {
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
          )

## If object is of class integer arguments rad and coef should be provided
## setMethod("ppsad",
##           signature(x="integer", sad="character", coef="list",
##                     trunc="ANY", plot="ANY", line="ANY"),
##           function(x, sad , coef, trunc=NA, plot=TRUE, line=TRUE, ...){
##               y <- as.numeric(x)
##               ppsad(x=y, sad=sad, coef=coef, trunc=trunc, plot=plot, line=line, ...)
##           }
##           )

## If argument x is fitsad class, arguments sad and coef should be missing
setMethod("ppsad",
          signature(x="fitsad", sad="missing", coef="missing", trunc="missing"),
          function (x, sad, coef, trunc=NA, plot=TRUE, line=TRUE, ...) {          
              sad <- x@sad
              coef <- as.list(bbmle::coef(x))
              trunc <- x@trunc
              y <- x@data$x
              ppsad(x=y, sad=sad, coef=coef, trunc=trunc, plot=plot, line=line, ...)
          }
          )

## Generic function and methods for pprad ##
setGeneric("pprad",
def = function(x, rad, coef, trunc=NA, plot=TRUE, line=TRUE, ...) standardGeneric("pprad"))

## If argument is of class rad arguments rad and coef should be provided
setMethod("pprad",
          signature(x="rad", rad="character", coef="list"),
          function (x, rad, coef, trunc=NA, plot=TRUE, line=TRUE, ...) {
              rad.tab <- x
              pr <- cumsum(rad.tab$abund/sum(rad.tab$abund))
              if(rad=="zipf"||rad=="mand") coef <- c(list(N=length(pr)),coef)
              if(rad=="gs") coef <- c(list(S=length(pr)),coef)
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
)

## If argument is of class numeric arguments rad and coef should be provided
setMethod("pprad",
          signature(x="numeric", rad="character", coef="list"),
          function (x, rad, coef, trunc=NA, plot=TRUE, line=TRUE, ...) {
              y <- rad(x)
              pprad(x=y, rad=rad, coef=coef, trunc=trunc, plot=plot, line=line, ...)
          }
          )

## If argument is of class integer arguments rad and coef should be provided
## setMethod("pprad",
##           signature(x="integer", rad="character", coef="list",
##                     trunc="ANY", plot="ANY", line="ANY"),
##           function (x, rad, coef, trunc=NA, plot=TRUE, line=TRUE, ...) {
##               y <- as.numeric(x)
##               pprad(x=y, rad=rad, coef=coef, trunc=trunc, plot=plot, line=line, ...)
##           }
##           )

## If argument is of class fitrad arguments rad and coef should be missing
setMethod("pprad",
          signature(x="fitrad", rad="missing", coef="missing"),
          function (x, rad, coef, trunc=NA, plot=TRUE, line=TRUE, ...) {
              rad <- x@rad
              coef <- as.list(bbmle::coef(x))
              trunc <- x@trunc
              y <- x@rad.tab
              pprad(x=y, rad=rad, coef=coef, trunc=trunc, plot=plot, line=line, ...)
          }
          )

## Generic function and methods for trueLL ##
setGeneric("trueLL",
           def = function(x, dens, coef, trunc, dec.places = 0, log = TRUE, ...) standardGeneric("trueLL")
           )

## If x is numeric arguments dens, coef and decimal places should be provided
setMethod("trueLL",
          signature(x="numeric", dens="character", coef="list", dec.places = "numeric", log="ANY"),
          function(x, dens, coef, trunc, dec.places = 0, log = TRUE, ...){
              dots <- list(...)
              D <- 10^(-dec.places)/2
              x <- round(x, dec.places)
              if(missing(trunc)||is.na(trunc)){
                  cdf <- get(paste("p", dens, sep=""), mode = "function")
                  probs <- do.call(cdf, c(list(q = x+D), as.list(coef), dots)) - do.call(cdf, c(list(q = x-D), as.list(coef), dots))
              }
              else{
                  probs <- do.call(ptrunc, c(list(dens, q = x+D, coef = as.list(coef), trunc=trunc), dots))-
                      do.call(ptrunc, c(list(dens, q = x-D, coef = as.list(coef), trunc=trunc), dots))
              }
              if(log) sum(log(probs)) else prod(probs)
          }
          )

## If x is of class fitsad only argument dec.places should be provided
setMethod("trueLL",
          signature(x="fitsad", coef="missing", trunc="missing", dec.places = "numeric", log="ANY"),
          function(x, dens, coef, trunc, dec.places, log, ...){
              if(x@distr != "C") stop("trueLL only makes sense for continuous distributions models")
              trueLL(x = x@data$x, dens = x@sad, coef = as.list(x@coef),
                               trunc=x@trunc, dec.places = dec.places, log, ...)
          }
          )
