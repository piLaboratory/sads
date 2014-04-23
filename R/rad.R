rad <- function(x){
  if(is(x, "fitsad"))
    y <- new("rad", data.frame(rank=1:length(x@data$x), abund=sort(x@data$x, decreasing=T)))
  else if(is(x,"fitrad"))
    y <- x@rad.tab
  else if(is(x,"numeric"))
    y <- new("rad", data.frame(rank=1:length(x), abund=sort(x, decreasing=T)))
  return(y)
}
