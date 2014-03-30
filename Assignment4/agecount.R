agecount = function(age=NULL){
  ## Check that "age" is non-NULL; else throw error
  if(is.null(age)) stop("age cannot be NULL")
  
  homicides = readLines("homicides.txt")
  
  p <- paste0(" ", age, " *?years*? old *?<")
  return(length(grep(pattern=p, x=homicides)))
}