rankhospital <- function(state, outcome, num = "best") {
    # Named vector to hold validations and index for argument outcome
    outcomes <- c("heart attack"=11, "heart failure"=17, "pneumonia"=23)
    
    # Validate argument outcome
    if (!outcome %in% names(outcomes)){
        stop("invalid outcome")
    } 
    
    # Read csv data
    outcomeFile <- read.csv("outcome-of-care-measures.csv", stringsAsFactors=FALSE)
    
    # Validate argument state
    if (!state %in% outcomeFile[,7]){
        stop("invalid state")
    } 
    
    # Named vector to hold validations and index for argument num
    if (num != "best" & num != "worst" & !is.numeric(num)){
        stop("invalid rank argument")
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
    
    # Return hospital name in the requested rank
    
    if (num == "best") {
        index <- 1
    } else if (num == "worst") {
        index <- nrow(outcomeResult)
    } else {
        index <- num}
    
    outcomeResult[index,1]
}