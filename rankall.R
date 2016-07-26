rankall <- function(outcomev, num1 = "best"){
    
    #Reads Data
    
    outcome.data <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE)
    
    #Terminates program for invalid arguments
    
    if(!(outcomev == "heart attack" | outcomev == "heart failure" | outcomev == "pneumonia")){
        
        stop("invalid outcome")
    }
    
    #Caches the index of the outcomev
    
    index <- ifelse(outcomev == "heart attack", 11, ifelse(outcomev == "heart failure", 17, 23))
    
    processed.data <- outcome.data[, c(2, 7, index)]
    
    colnames(processed.data) <- c("hospital", "state", "outcome")
    
    #Ranks hospitals based on outcome and breaks ties by alphabetical order
    
    processed.data <- processed.data[order(processed.data$outcome, processed.data$hospital), ]
    
    splitData <- split(processed.data, processed.data$state)
    
    #Assigns num1 character values to integers
    
    if(num1 == "best"){
        
        num1 <- 1
    }
    
    else{
        
        a <- "this is random"
    }
    
    #Returns a list of hospitals from every state by the specified rank
    
    ranked.hospitals <- lapply(splitData, function(anonymous) anonymous[num1, 1])
    
    df <- cbind(ranked.hospitals, names(ranked.hospitals))
    
    colnames(df) <- c("hospitals", "state")
    
    df
    
}