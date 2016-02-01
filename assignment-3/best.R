best <- function(state, outcome) {
    ## Read outcome data
    outcomes <- read.csv("data/assignment-3/outcome-of-care-measures.csv", colClasses="character")
    
    
    ## Check that state and outcome are valid
    if(all(!outcomes$State == state)){
        stop("invalid state")
    }
    
    subset_outcomes = outcomes[outcomes$State == state,]
    
    # the way to check whether the outcome is valid is also by using the keys     
    # turn the outcome into string that has the first letter cap.
    words <- strsplit(outcome, " ")[[1]]
    key_tail <- paste(toupper(substring(words, 1, 1)), substring(words, 2), sep="", collapse=".")
    key <- paste('Hospital.30.Day.Death..Mortality..Rates.from', key_tail, sep=".")
    
    # if the key doesn't exist, than it's invalid     
    if(!key %in% colnames(outcomes)){
        stop("invalid outcome")
    }
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    subset_outcomes[[key]] <- as.numeric(subset_outcomes[[key]])
    best_value <- min(subset_outcomes[[key]], na.rm=TRUE)
    
    # get the index of that min val     
    idx <- subset_outcomes[[key]] == best_value
    
    # replace all na     
    idx <- replace(idx, is.na(idx), FALSE)
    
    return(subset_outcomes[idx,]$Hospital.Name)
    
}