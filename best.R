## Finds the best hospital in the state for a given outcome
## Args:
##    state: The state to search (by 2 letter acronym)
##    outcome: One of "heart attack", "heart failure", "pneumonia"
best <- function(state, outcome) {
    ## Read outcome data
    outcome_csv <- file.path("Data", "outcome-of-care-measures.csv")
    df <- read.csv(outcome_csv, colClasses = "character")

    ## Check that state and outcome are valid
    if (!(state %in% df$State)) {
        stop(sprintf("'%s' not a valid state", state))
    }
    lookup <- c(11, 17, 23)  # columns 
    names(lookup) <- c("heart attack", "heart failure", "pneumonia")
    if (!(outcome %in% names(lookup))) {
        stop(sprintf("'%s' not a valid outcome", outcome))
    }

    ## Return hospital name in that state with lowest 30-day death
    ## rate
    state_df <- df[df$State == state, ]
    state_df[which.min(state_df[[lookup[[outcome]]]]), "Hospital.Name"]
}