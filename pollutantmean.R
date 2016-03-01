pollutantmean <- function(directory, pollutant, id=1:332){
    filenamelist <- list.files(path = directory, full.names = TRUE) # get the all the files into a vector
    data <- numeric()
    for (i in id) {
        targetcsv <- read.csv(filenamelist[i]) #read the target file data according to the ids
        data <- c(data, targetcsv[[pollutant]]) #concatenate the target data of specific pollutant
    }
    mean(data, na.rm = TRUE)
}

