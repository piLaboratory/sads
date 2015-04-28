is.wholenumber	<-function(x, tol = .Machine$double.eps^0.5){
  abs(x - round(x)) < tol
}

# bissection algorith for finding the quantile of a *discrete*
# and *unbounded* probability distribution function
qfinder <- function(dist, want, coef) {
	f <- get(paste("p",dist, sep=""), mode="function")
	guess <- 1
	last <- 0
	# "phase 0": is 1 overshooting? Is q = Inf?
	if (do.call(f, c(q=0, coef)) >= want) return (0);
	if (do.call(f, c(q=1, coef)) >= want) return (1);
    if (want >= 0.999999999999999999) return (Inf); 
	# phase 1: double the guess until you overshoot
	repeat {
		my.q <- do.call(f, c(q=guess, coef))
		if (my.q > want) break;
		# Is there a bug in pdist?
		if (my.q == last) stop ("quantile function did not converge!")
		last = my.q
		guess <- 2*guess
	}
	# Phase 2: bissect
	lower <- guess/2
	upper <- guess
	repeat {
		if (upper-lower == 1) break;
		guess <- round((lower + upper)/2)
		my.q <- do.call(f, c(q=guess, coef))
		if(my.q > want) upper <- guess else lower <- guess
	}
	# Phase 3: lower or upper?
	if (do.call(f, c(q=lower, coef)) < want) upper else lower
}
