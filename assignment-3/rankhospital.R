rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    outcomes <- read.csv("data/assignment-3/outcome-of-care-measures.csv", colClasses="character")
    
    ## Check that state and outcome are valid
    if(all(!outcomes$State == state)){
        stop("invalid state")
    }
    
    subset_outcomes <- outcomes[outcomes$State == state,]
    
    # the way to check whether the outcome is valid is also by using the keys     
    # turn the outcome into string that has the first letter cap.
    words <- strsplit(outcome, " ")[[1]]
    key_tail <- paste(toupper(substring(words, 1, 1)), substring(words, 2), sep="", collapse=".")
    key <- paste('Hospital.30.Day.Death..Mortality..Rates.from', key_tail, sep=".")
    # print(key)
    
    # if the key doesn't exist, than it's invalid     
    if(!key %in% colnames(outcomes)){
        stop("invalid outcome")
    }
    
    # exclude the hospitals without this outcome data.
    # assuming there are cases where values = na, and "" (empty string) and Not Available
    subset_outcomes <- subset_outcomes[subset_outcomes[key] != "" & !is.na(subset_outcomes[key]) & subset_outcomes[key] != "Not Available", ]
    # print(head(subset_outcome))
    
    # return error when the asked num is our of range    
    if(length(subset_outcomes) < num && num != "best" & num != "worst"){
        return (NA)
    }
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    # sort it according to the given outcome
    ordered_subset <- subset_outcomes[order( as.numeric(subset_outcomes[[key]]), subset_outcomes$Hospital.Name), ]
    
    # get the name from that index number 
    if(num == "worst"){
        # dim returns [number of rows, number of columns]
        # we return the number of rows.
        num <- dim(ordered_subset)[1]
    }
    if(num == "best"){
        num <- 1
    }
    
    ranked_num_subset <- ordered_subset[num,]
    return(ranked_num_subset$Hospital.Name)
    
}