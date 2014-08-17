corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    
    readfile <- function(id) {
        read.csv(paste(c(directory, "/", sprintf("%03d",id), ".csv"), collapse=""))
    }
    
    cor_vec <- numeric()
    
    for (ii in 1:332) {
        MyData <- na.omit(readfile(ii))
        nobs <- nrow(MyData)
        if (nobs > threshold) {
            cor_vec <- c(cor_vec, cor (MyData["sulfate"], MyData["nitrate"]))
        }
    }
    
    return(cor_vec)
}