rankall <- function(outcome, num = "best") {
    # Named vector to hold validations and index for argument outcome
    outcomes <- c("heart attack"=11, "heart failure"=17, "pneumonia"=23)
    
    # Validate argument outcome
    if (!outcome %in% names(outcomes)){
        stop("invalid outcome")
    } 
    
    # Validate argument num
    if (num != "best" & num != "worst" & !is.numeric(num)){
        stop("invalid rank argument")
    }
    
    # Read csv data
    outcomeFile <- read.csv("outcome-of-care-measures.csv", stringsAsFactors=FALSE)
    
    # Read only the needed columns
    outcomeResult <- outcomeFile[,c(2, 7, outcomes[outcome])]
    
    # Rename the resulting file columns
    names(outcomeResult) <- c("Hospital", "State", "Outcome")
    
    # Selecting only the required state and valid rows
    outcomeResult <- subset(outcomeResult, Outcome != "Not Available")
    
    # Change the outcome to numeric
    outcomeResult$Outcome <- as.numeric(outcomeResult$Outcome)
    
    # Get a list of all the states
    states <- sort(unique(outcomeResult$State))
    
    # Initialize final result
    hospitals <- data.frame(Hospital=character(), State=character(), stringsAsFactors=FALSE)
    
    # Loop throught the states to get the requested rank
    for(state in states){
        # Get only one state at a time
        tempResult <- subset(outcomeResult, State == state)
        
        # Sort the hospitals in the state
        tempResult <- tempResult[order(tempResult$Outcome, tempResult$Hospital),]
        
        # Rank the hospitals in the state
        if (num == "best") {
            index <- 1
        } else if (num == "worst") {
            index <- nrow(tempResult)
        } else {
            index <- num}
        
        hospitals <- rbind(hospitals, data.frame("Hospital"=tempResult[index,1], "State"=state))
    }
    
    hospitals
}