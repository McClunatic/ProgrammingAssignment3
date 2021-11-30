## Ranks hospitals in all states for a given outcome
## Args:
##     outcome: The outcome, "heart attack", "heart failure", or "pneumonia"
##     num: Rank number or one of "best", "worst"
## Returns:
##     Data frame of ranked hospitals by state
rankall <- function(outcome, num = "best") {
    ## Read outcome data
    outcome_csv <- file.path("Data", "outcome-of-care-measures.csv")
    df <- read.csv(outcome_csv, colClasses = "character")

    ## Check that outcome is valid
    index <- names(df)[c(2, 11, 17, 23)]  # columns
    names(index) <- c("hospital", "heart attack", "heart failure", "pneumonia")
    if (!(outcome %in% names(index)[2:4])) {
        stop(sprintf("'%s' not a valid outcome", outcome))
    }

    ## Check that rank is valid
    if (!is.numeric(num) && !is.integer(num)) {
        if (!(num %in% c("best", "worst"))) {
            stop(sprintf("'%s' not a valid num", num))
        }
    }

    ## For each state, find the hospital of the given rank
    outc <- index[[outcome]]
    hosp <- index[["hospital"]]

    # Reduce df to 3 columns, recast outcome as numeric, drop NAs
    df <- df[, c(outc, hosp, "State")]
    suppressWarnings(df[, 1] <- as.numeric(df[, 1]))
    df <- df[complete.cases(df), ]

    # Loop over unique state names, store rows in outdfs
    states <- unique(df$State)
    outdfs <- vector(mode = "list", length = length(states))
    outcols <- c(hosp, "State")
    for (i in seq_along(states)) {
        sdf <- df[df$State == states[i], ]
        row <- if (is.numeric(num) || is.integer((num))) {
            num
        } else if (num == "worst") {
            nrow(sdf)
        } else {
            1
        }
        outdfs[[i]] <- sdf[order(sdf[[outc]], sdf[[hosp]]), ][row, outcols]
        outdfs[[i]][, "State"] <- states[i]
    }

    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    res <- do.call("rbind", outdfs)
    rownames(res) <- res[, "State"]
    res[order(res[, "State"]), ]
}