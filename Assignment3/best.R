best = function(state, outcome) {
  o <- read.csv("outcome-of-care-measures.csv", colClasses="character")
  
  col.index <- c(11, 17, 23)
  names(col.index) <- c("heart attack", "heart failure", "pneumonia")
  cols = c(which(names(o)=="Hospital.Name"), col.index[outcome]) #the only two cols we need
  
  if(!state %in% unique(o$State)) stop("invalid state")
  if(!outcome %in% names(col.index)) stop("invalid outcome")
  
  o <- o[which(o$State == state), cols]
  o <- o[complete.cases(o), ]
  suppressWarnings(o[, 2] <- as.numeric(o[, 2]))
  o <- o[order(o$Hospital.Name),]
  return(o$Hospital.Name[which.min(o[, 2])])
}
