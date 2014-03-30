rankall = function(outcome, num="best") {
  
  # create named vector to pick column index
  col.index <- c(11, 17, 23)
  names(col.index) <- c("heart attack", "heart failure", "pneumonia")
  
  # stopping rule
  if(!outcome %in% names(col.index)) stop("invalid outcome")
  
  # read only Hospital.Name, State, and the outcome column
  
  # set vector of colClasses, initiated at NULL (don't read)
  cc <- rep("NULL", 46)
  
  # set only columns 2, 7, and the outcome column to character
  cc[c(2, 7, col.index[outcome])] <- "character"
  
  # read only these three columns (more efficient on memory)
  o <- read.csv("outcome-of-care-measures.csv", colClasses=cc)
  
  # convert outcome measure to numeric and remove NAs
  suppressWarnings(o[, 3] <- as.numeric(o[, 3]))
  o <- o[complete.cases(o), ]
  
  # define workhorse function for by() below
  get.hospital = function(x, num) {
    
    # convert character num to an index value
    if(num=="best") num <- 1
    if(num=="worst") num <- nrow(x)
    if(num > nrow(x)) return(NA)
    
    # tricky part
    # sort data by 3rd column and return the num-th entry of Hospital.Name
    return(x[order(x[, 3], x$Hospital.Name), "Hospital.Name"][num])
  }
  
  t <- by(o, o$State, get.hospital, num)  ## returns class "by"
  state.names <- dimnames(t)[[1]] ## attribute dimnames is a list of one entry
  return(data.frame(hospital=as.character(t), state=state.names, row.names=state.names))
}
