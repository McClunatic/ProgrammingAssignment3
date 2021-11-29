## Ranks hospitals in a state for a given outcome
## Args:
##     state: The state to rank hospitals for
##     outcome: The outcome, "heart attack", "heart failure", or "pneumonia"
##     num: Rank number or one of "best", "worst"
## Returns:
##     Name of ranked hospital
rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    outcome_csv <- file.path("Data", "outcome-of-care-measures.csv")
    df <- read.csv(outcome_csv, colClasses = "character")

    ## Check that state and outcome are valid
    if (!(state %in% df$State)) {
        stop(sprintf("'%s' not a valid state", state))
    }
    index <- names(df)[c(2, 11, 17, 23)]  # columns
    names(index) <- c("hospital", "heart attack", "heart failure", "pneumonia")
    if (!(outcome %in% names(index)[2:4])) {
        stop(sprintf("'%s' not a valid outcome", outcome))
    }

    # Reduce df to state df "sdf", names(outcome, hospital name)
    outc <- index[[outcome]]
    hosp <- index[["hospital"]]
    sdf <- df[df$State == state, c(outc, hosp)]
    suppressWarnings(sdf[, 1] <- as.numeric(sdf[, 1]))
    sdf <- sdf[complete.cases(sdf), ]

    ## Check that rank is valid
    if (!is.numeric(num) && !is.integer(num)) {
        if (!(num %in% c("best", "worst"))) {
            stop(sprintf("'%s' not a valid num", num))
        } else {
            num <- if (num == "best") {
                1
            } else {
                nrow(sdf)
            }
        }
    }

    ## Return hospital name in that state with the given rank
    sdf[order(sdf[[outc]], sdf[[hosp]]), ][num, hosp]
}
