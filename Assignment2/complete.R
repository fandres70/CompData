complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    if(!existsFunction("getmonitor")) source("getmonitor.R")
    
    data <- data.frame()
    for(i in id){
        temp <- getmonitor(i, directory)
        nobs <- sum(complete.cases(temp))
        data <- rbind(data, c(as.integer(i), nobs))
    }
    names(data) <- c("id", "nobs")
    return(data)
}