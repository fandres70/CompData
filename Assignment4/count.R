count <- function(cause=NULL){
  ## Check that "cause"is non-NULL; else throw error
  if(is.null(cause)) stop("cause cannot be NULL")
  
  ## Check that specific "cause" is allowed; else throw error
  if(cause=="asphyxiation") p <- ">[C|c]ause: *[A|a]sphyxiation"
  else  if(cause=="blunt force") p <- ">[C|c]ause: *[B|b]lunt *?[F|f]orce"
  else if(cause=="other") p <- ">[C|c]ause: *[O|o]ther"
  else if(cause=="shooting") p <- ">[C|c]ause: *[S|s]hooting"
  else if(cause=="stabbing") p <- ">[C|c]ause: *[S|s]tabbing"
  else if(cause=="unknown") p <- ">[C|c]ause: *[U|u]nknown"
  else stop("cause value is not valid")
  
  homicides <- readLines("homicides.txt")
  
  return(length(grep(pattern=p, x=homicides)))
}