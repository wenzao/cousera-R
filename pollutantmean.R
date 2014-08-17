pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    
    readfile <- function(id) {
        read.csv(paste(c(directory, "/", sprintf("%03d",id), ".csv"), collapse=""))
    }
    
    for (ii in 1:length(id)) {
        if (ii==1) {MyData <- readfile(id[ii])}
        else {MyData <- rbind(MyData, readfile(id[ii]))}
    }
    
    mean(MyData[[pollutant]],na.rm=TRUE)
}
