#' Continuous or discrete distributions
#'
#' Checks if the distribution is continuous or discrete
#'
#' Returns whether a given distribution (used for a sad or rad model) is "discrete"
#' or "continuous". The name is compared to a list of known distributions, so distributions
#' not used in the sads package will return "NA" with a warning.
#' 
#' @param distribution Character. The name of the distribution ("geom" for "fitgeom", "weibull" for "fitweibull", etc.
distr <- function(distribution) {
  if (class(distribution)!="character") stop("Distribution must be from class character")
  if (distribution %in% c("bs", "lnorm", "gamma", "pareto", "weibull"))
    return("continuous")
  if (distribution %in% c("gs", "geom", "rbs", "power", "poilog", "nbinom", "mzsm", "mand", 
                          "ls", "volkov", "zipf"))
    return("discrete")
  # if arrived here...
  warning(paste("Unknown distribution:", distribution))
  return(NA)
}
