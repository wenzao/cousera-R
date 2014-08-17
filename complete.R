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
    
    readfile <- function(id) {
        read.csv(paste(c(directory, "/", sprintf("%03d",id), ".csv"), collapse=""))
    }
    
    nobs <- numeric(length(id))
    
    for (ii in 1:length(id)) {
        MyData <- readfile(id[ii])
        nobs[ii] <- sum(complete.cases(MyData))
    }
    
    data.frame(id, nobs)
    
}