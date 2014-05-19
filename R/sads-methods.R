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
          function(x, ...){
            dots <- list(...)
            x.hist <- rep(as.integer(as.character(x$octave)), as.integer(as.character(x$Freq)))
            if(!"col" %in% names(dots)) dots$col = "gray"
            if(!"main" %in% names(dots)) dots$main = ""
            if(!"ylab" %in% names(dots)) dots$ylab = "N of species"
            if(!"xlab" %in% names(dots)) dots$xlab = "Abundance class"
            if(!"axes" %in% names(dots)){ 
              do.call(hist, c(list(x=x.hist,
                   breaks = c((min(as.integer(as.character(x$octave)))-1),as.integer(as.character(x$octave))),
                                   axes=FALSE),dots))
              axis(2)
              n <- as.numeric(as.character(x[,1]))
              axis(1,at=n[seq(1,length(x[,1]),2)],
                   labels=x[seq(1,length(x[,1]),2),2])
            }
            else
              do.call(hist, c(list(x=x.hist,
                   breaks = c((min(as.integer(as.character(x$octave)))-1),as.integer(as.character(x$octave)))),dots))
          }
          )

setMethod("points","octav",
          function(x,...){
            dots <- list(...)
            if(!"type" %in% names(dots)) dots$type="b"
            if(!"col" %in% names(dots)) dots$col="blue"
            X <- c((min(as.integer(as.character(x$octave)))-1), as.integer(as.character(x$octave)))
            X <- X[-length(X)]+diff(X)/2
            do.call(points, c(list(x = X, y = x$Freq), dots))
          }
          )

setMethod("lines","octav",
          function(x,...){
            dots <- list(...)
            if(!"type" %in% names(dots)) dots$type="b"
            if(!"col" %in% names(dots)) dots$col="blue"
            X <- c((min(as.integer(as.character(x$octave)))-1), as.integer(as.character(x$octave)))
            X <- X[-length(X)]+diff(X)/2
            do.call(lines, c(list(x = X, y = x$Freq), dots))
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
              val <- -2 * logLiks + k * df * (df + 1)/(nobs - df - 
                                                       1)
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
            coef <- as.list(bbmle::coef(object))
            trunc <- object@trunc
            distr <- object@distr
            rad <- object@rad
            x <- object@rad.tab$abund
            N <- sum(x)
            S <- length(x)
            y <- 1:S
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
setMethod("radpred",signature(object="missing", sad="missing", rad="character",
                              coef="list", distr="missing", S="numeric", N="numeric"),
          function(object, sad, rad, coef, trunc, distr, S, N, ...){
            dots <- list(...)
            y <- 1:S
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
setMethod("radpred",signature(object="missing", sad="character", rad="missing",
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

          
## ocatvpred generic functions and methods ###

setGeneric("octavpred",
def = function(object, sad, rad, coef, trunc, oct, S, N, ...) standardGeneric("octavpred"))

## if object is of class fitsad (no other argument should be provided)
setMethod("octavpred", signature(object="fitsad",sad="missing", rad="missing",
                                 coef="missing", trunc="missing", oct="ANY",
                                 S="missing", N="missing"),
          function(object, ...){
            dots <- list(...)
            coef <- as.list(bbmle::coef(object))
            trunc <- object@trunc
            sad <- object@sad
            x <- object@data$x
            S <- length(x)
            N <- sum(x)
            if(missing(oct)){
              oct <- 1:(ceiling(max(log2(x)))+1)
              if(any(x < 1)){
                octlower <- ceiling(min(log2((x)))+1):0
                oct <- c(octlower, oct)
              }
            }
            n <- 2^(oct-1)
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
          function(object, ...){
            dots <- list(...)
            coef <- as.list(bbmle::coef(object))
            trunc <- object@trunc
            rad <- object@rad
            x <- object@rad.tab$abund
            S <- length(x)
            N <- sum(x)
            if(missing(oct)){
              oct <- 1:(ceiling(max(log2(x)))+1)
              if(any(x < 1)){
                octlower <- ceiling(min(log2((x)))+1):0
                oct <- c(octlower, oct)
              }
            }
            n <- 2^(oct-1)
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
            if(missing(oct)){
              oct <- 1:(ceiling(max(log2(x)))+1)
              if(any(x < 1)){
                octlower <- ceiling(min(log2((x)))+1):0
                oct <- c(octlower, oct)
              }
            }
            n <- 2^(oct-1)
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
              oct <- 1:(ceiling(max(log2(x)))+1)
              if(any(x < 1)){
                octlower <- ceiling(min(log2((x)))+1):0
                oct <- c(octlower, oct)
              }
            }
            n <- 2^(oct-1)
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
            n <- 2^(oct-1)
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
            n <- 2^(oct-1)
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
