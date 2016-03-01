complete <- function(directory, id = 1:322) {
    ## 'directory' is a character vector of length 1 indicating the location of the CSV files
    ## 'id' is an integer vector indicating the monitor ID numbers to be used
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## where 'id' is the monitor ID number and 'nobs' is the number of complete cases
    filenamelist <- list.files(path = directory, full.names = TRUE) # get the all the files into a vector
    dataframe <- data.frame()
    for (i in id) {
        targetcsv <- read.csv(filenamelist[i]) #read the target file data according to the id
        nobs <- sum(complete.cases(targetcsv)) #calculate the total complete cases of each id
        eachnode <- data.frame(i, nobs) #construct each node
        dataframe <- rbind(dataframe, eachnode) #bind each node
    }
    dataframe
}