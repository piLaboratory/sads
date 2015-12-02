is.wholenumber	<-function(x, tol = .Machine$double.eps^0.5){
  abs(x - round(x)) < tol
}

# bissection algorith for finding the quantile of a *discrete*
# and *unbounded* probability distribution function
qfinder <- function(dist, want, coef, init = 0) {
	# "phase 0": are the params invalid? is "init" overshooting? Is q = Inf?
	if (want < 0) return (0);
  if (want >= 0.999999999999999999) return (Inf); 
	q0 <- sum(do.call(dist, c(list(x=0:init), coef)))
	if (is.nan(q0)) return (NaN);
	if (q0 >= want) return (init);
	# phase 1: double the guess until you overshoot
  step <- 1
	guess <- init+2
	last <- init+1
  cum <- q0
	repeat {
		my.q <- do.call(dist, c(list(x=last:guess), coef))
    my.sq <- sum(my.q)
		if (cum + my.sq > want) break;
		# Is there a bug in pdist?
		if (my.sq < 1e-8) stop ("quantile function did not converge!")
		last <- guess+1
    step <- step * 2
		guess <- guess + step
    cum <- cum + my.sq
	}
	# Phase 2: exhaustive examine last interval
  my.sq <- cum + cumsum(my.q)
  add <- which(my.sq < want - .Machine$double.eps)
  if (length(add)) last <- last + max(add)
  return(last)
}

# Wrapper for cumsum, converting NA to NaN for coherence with [dpqr] functions and
# padding with 0 for 1-based distributions such as gs
cumsumW <- function(dist, q, coef, lower.tail, log.p, pad) {
	if (pad)
		z <- c(0, cumsum(do.call(dist, c(list(x=1:max(q)), coef))))
	else
		z <- cumsum(do.call(dist, c(list(x=0:max(q)), coef)))
	y <- z[q+1]
	if(!lower.tail) y <- 1-y
	if(log.p) y <- log(y)
	y[is.na(y)] <- NaN
	return (y)
}
