corr <- function(directory, threshold = 0) {
# 'directory' is a character vector of length 1 indicating the location of the CSV files
# 'threshold' is a numeric vector of length 1 indicating the number of completely observed observations (on all
# variables) required to compute the correlation between nitrate and sulfate; the default is 0
# Return a numeric vector of correlations
# NOTE: Do not round the result!
    
    filenamelist <- list.files(path = directory, full.names = TRUE)
    coldata <- numeric()
    for(i in 1: length(filenamelist)){
        targetcsv <- read.csv(filenamelist[i])
        #calculate the complete cases on all variables
        isallobserved <- !is.na(targetcsv$sulfate)&!is.na(targetcsv$nitrate)
        if(sum(isallobserved) > threshold){
            coldata <- c(coldata, cor(targetcsv$sulfate[isallobserved], targetcsv$nitrate[isallobserved]))
        }
    }
    coldata
}