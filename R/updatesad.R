## updates a fitsad/fitrad object by running the optimizer again starting
## on better fit returned by profile
updatesad <- function(object, ...) {
	dots <- as.list(...)
	prof <- profile(object)
	if(class(prof) == "profile.mle2") stop("Cannot update, profile did not find a better fit!")
	newcall <- as.list(object@call)
	newcall[[1]] <- NULL # removes the "mle2" function name from the call
	newcall[["control"]] <- NULL # removes the "control" slot
	# merges the original call with arguments supplied by ...
	for (v in names(dots)) newcall[[v]] <- dots[[v]]
	names(newcall$start) -> name # bugfix? profile returns coefficient "prob" name as "prob.prob"
	newcall$start <- list(prof@fullcoef) # TODO: might be coef instead? how to deal with fixed?
	names(newcall$start) <- name
	newobj <- do.call("mle2", newcall)
	if(class(object) == "fitsad") 
		return (new("fitsad", newobj, sad=object@sad, distr=object@distr, trunc=object@trunc))
	else # fitrad
		return (new("fitrad", newobj, rad=object@rad, distr=object@distr, trunc=object@trunc, rad.tab=object@rad.tab))
}
updaterad <- function(object, ...) updatesad(object, ...)
