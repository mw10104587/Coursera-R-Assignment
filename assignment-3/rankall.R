rankall <- function(outcome, num = "best") {
    ## Read outcome data
    outcomes <- read.csv("data/assignment-3/outcome-of-care-measures.csv", colClasses="character")
        
    # the way to check whether the outcome is valid is also by using the keys     
    # turn the outcome into string that has the first letter cap.
    words = strsplit(outcome, " ")[[1]]
    key_tail <- paste(toupper(substring(words, 1, 1)), substring(words, 2), sep="", collapse=".")
    key <- paste('Hospital.30.Day.Death..Mortality..Rates.from', key_tail, sep=".")
    # print(key)
    
    # if the key doesn't exist, than it's invalid     
    if(!key %in% colnames(outcomes)){
        stop("invalid outcome")
    }
    
    
    if(num == "best"){
        num <- 1
    }

    
    # this function will be called by tapply, which means the input data will be in the format
    # Hospital.Name, key_value
    # but only returns Hospital.Name and state
    
    # we need to reference the num var in this function
    get_hospital_in_state = function(data){
        
        data <- data[data[[key]] != "Not Available",]
        data <- data[order(as.numeric(data[[key]]), data$Hospital.Name),]
        # print(head(data, 20))
        
        if(num == "worst"){
            num <- dim(data)[1]
        }
        
        data[num,]$Hospital.Name
        
    }
    
    hospitals <- lapply(split(outcomes, outcomes$State), get_hospital_in_state)
    # print(hospitals)
    hospitals <- data.frame(hospitals)
    result <- t(rbind(t(hospitals)[,1], names(hospitals)))
    # rownames(result) = NULL
    colnames(result) <- c("hospital", "state")    
    return(result)
    ## Return a data frame with the hospital names and the (abbreviated) state name
}