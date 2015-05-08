dpowbend <- function(x, s, omega, log = FALSE){
	if(omega == 0) return(dpower(x, s, log))
	s[ !is.finite(s) | s <= 1 ] <- NaN
	omega[ !is.finite(omega) | omega < 0 ] <- NaN
	y <- x^(-s) * exp(-omega * x) / LiE(s, -omega)
	if (any(is.nan(y))) warning ("NaNs produced")
	if (any(!is.wholenumber(x))) warning("non integer values in x")
	y[ ! is.wholenumber(x) | x < 1] <- 0
	if(log) return(log(y))
	else return(y)
}

ppowbend <- function(q, s, omega, lower.tail=TRUE, log.p=FALSE){
	if (any(!is.wholenumber(q))) warning("non integer values in q")
	y <- suppressWarnings(cumsumW("powbend", q, list(s=s, omega=omega), lower.tail, log.p, pad=FALSE))
	if(any(is.nan(y))) warning("NaNs produced")
	return(y)
}

qpowbend<-function(p, s, omega, lower.tail = TRUE, log.p = FALSE){
	if (log.p) p <- exp(p)
	if(!lower.tail) p <- 1 - p
	q <- function(p) suppressWarnings(qfinder("powbend", p, list(s=s, omega=omega)))
	y <- sapply(p, q)
	if(any(is.nan(y))) warning("NaNs produced")
	return(y)
}

fitpowbend <- function(x, trunc, start.value, ...){
  dots <- list(...)
  if (!missing(trunc)){
    if (min(x)<=trunc) stop("truncation point should be lower than the lowest data value")
  }
  if (missing(start.value)){
    gamahat <- function(ga, xvec) eq <- -sum(log(xvec)) - zeta(ga, deriv =1)*length(xvec)/zeta(ga)
    shat <- uniroot(gamahat, interval=c(1.01, 20), xvec = x)$root
	omegahat <- 0.5
  } else{
    shat <- start.value[1]
    omegahat <- start.value[2]
  }
  if(!"method" %in% names(dots)){
    dots$method <- "L-BFGS-B"
    if(!"lower" %in% names(dots)) dots$lower=c(s=1.001, omega=0.000001)
    if(!"upper" %in% names(dots)) dots$upper=c(s=2.999, omega=0.3)
  }
  if (missing(trunc)){
    LL <- function(s, omega) -sum(dpowbend(x, s, omega, log = TRUE))
  } else{
    LL <- function(s, omega) -sum(dtrunc("powbend", x = x, coef = list(s=s, omega=omega), trunc = trunc, log = TRUE))
  }
  result <- do.call("mle2", c(list(LL, start = list(s = shat, omega=omegahat), data = list(x = x)), dots))
  new("fitsad", result, sad = "powbend", distr = "D", trunc = ifelse(missing(trunc), NaN, trunc))
}
