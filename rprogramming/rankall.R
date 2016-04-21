rankall <- function(outcome, num = "best") {
    ## Read outcome data
    measures <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")
    dataframe <- data.frame()
    ## Check that the outcome is valid
    if (!outcome %in% c('heart attack', 'heart failure', 'pneumonia')) {
        stop('invalid outcome')
    }
    ## get the target outcome
    if (outcome == 'heart attack') {
        outcome_col <- 11
    } else if (outcome == 'heart failure') {
        outcome_col <- 17
    } else {
        outcome_col <- 23
    }
    ## For each state, find the hospital of the given rank
    for (state in unique(measures$State)) {
        state_outcome <- subset(measures, State == state)
        dataforsort <- state_outcome[, c(2, outcome_col)]
        datasorted <- dataforsort[order(as.numeric(dataforsort[,2]), dataforsort[,1], na.last = NA),]
        this.num <- num
        if (num == 'best') {
            this.num <- 1
        }
        if (num == 'worst') {
            this.num <- nrow(datasorted)
        }
        ## print the hospital with the ranking number
        dataframe <- rbind(dataframe, data.frame(datasorted[this.num, 1], state))
    }
    ## Return a data frame with the hospital names and the
    colnames(dataframe) <- c('hospital', 'state')
    dataframe[]
    ## (abbreviated) state name
}