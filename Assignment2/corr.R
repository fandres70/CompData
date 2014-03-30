corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    if(!existsFunction("getmonitor")) source("getmonitor.R")
    if(!existsFunction("complete")) source("complete.R")
    
    ids <- substr(dir(directory), 1, 3)  ##get first 3 letters of file list
    
    # get list of ids with higher number of obs than threshold
    obs <- complete(directory, ids)
    obs <- as.vector(obs[which(obs$nobs >= threshold), 1])
    
    correlations <- numeric() #initiate vector of correlations
    for(i in obs){
        temp <- getmonitor(i, directory)
        temp <- temp[complete.cases(temp), c("nitrate", "sulfate")]
        correlations <- c(correlations, cor(temp$sulfate, temp$nitrate))
    }
    return(correlations)
}