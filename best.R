best <- function(state = NA, outcome = NA) {
    
    ## Named vector to hold validations and index for argument outcome
    outcomes <- c("heart attack"=11, "heart failure"=17, "pneumonia"=23)
    
    ## Validate argument outcome
    if (!outcome %in% names(outcomes)){
        stop("invalid outcome")
    } 
    
    ## Read csv data
    outcomeFile <- read.csv("outcome-of-care-measures.csv", stringsAsFactors=FALSE)
    
    # Validate argument state
    if (!state %in% outcomeFile[,7]){
        stop("invalid state")
    } 
    
    # Read only the needed columns
    outcomeResult <- outcomeFile[,c(2, 7, outcomes[outcome])]

    # Rename the resulting file columns
    names(outcomeResult) <- c("Hospital", "State", "Outcome")
    
    # Selecting only the required state and valid rows
    outcomeResult <- subset(outcomeResult, State == state & Outcome != "Not Available")
    
    # Change the outcome to numeric
    outcomeResult$Outcome <- as.numeric(outcomeResult$Outcome)
    
    # Sort to find the best hospital
    outcomeResult <- outcomeResult[order(outcomeResult$Outcome, outcomeResult$Hospital),]

    # Return hospital name in that state with lowest 30-day death
    outcomeResult[1,1]


    ## rate
}

# named vector with something like 
# outcomes <- c(“heart attack”=11, “heart failure”=17, “pneumonia”=23) 
# then you can use that to both test the function argument and select the column. 
# Something like df[, c(2,7,outcomes[outcome])]. 
# Also, when you validate the outcome argument instead of using %in% outcomes you’d use %in% names(outcomes).