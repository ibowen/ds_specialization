best <- function(state, outcome) {
    ## Read outcome data
    measures <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")
    ## Check that state and outcome are valid
    if (!state %in% measures$State) {
        stop('invalid state')
    }
    if (!outcome %in% c('heart attack', 'heart failure', 'pneumonia')) {
        stop('invalid outcome')
    }
    ## Return hospital name in that state with lowest 30-day death
    if (outcome == 'heart attack') {
        outcome_col <- 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack'
    } else if (outcome == 'heart failure') {
        outcome_col <- 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure'
    } else {
        outcome_col <- 'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia'
    }
    state_outcome <- subset(measures, State == state)
    outcome_col <- as.numeric(state_outcome[[outcome_col]])
    hospital_col <- state_outcome[["Hospital.Name"]]
    min <- min(outcome_col, na.rm = TRUE)
    minpos <- which(outcome_col == min)
    hospitalname <- hospital_col[minpos]
    ## rate
    min(hospitalname)
}