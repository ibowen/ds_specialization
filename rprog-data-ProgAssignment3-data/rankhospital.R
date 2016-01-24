rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    measures <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")
    ## Check that state and outcome are valid
    if (!state %in% measures$State) {
        stop('invalid state')
    }
    if (!outcome %in% c('heart attack', 'heart failure', 'pneumonia')) {
        stop('invalid outcome')
    }
    ## Return hospital name in that state with the given rank
    if (outcome == 'heart attack') {
        outcome_col <- 11
    } else if (outcome == 'heart failure') {
        outcome_col <- 17
    } else {
        outcome_col <- 23
    }
    ## get the data with the State from the argument state
    state_outcome <- subset(measures, State == state)
    ## extract the columns from state_outcome with only the hospital name and outcome
    dataforsort <- state_outcome[, c(2, outcome_col)]
    ## sort the data by rates and then the hospital names
    datasorted <- dataforsort[order(as.numeric(dataforsort[,2]), dataforsort[,1], na.last = NA),]
    ## handle the best and worst case
    if (num == 'best') {
        num <- 1
    }
    if (num == 'worst') {
        num <- nrow(datasorted)
    }
    ## print the hospital with the ranking number
    datasorted[num, 1]
}