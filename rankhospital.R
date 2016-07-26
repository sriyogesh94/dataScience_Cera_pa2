rankhospital <- function(statev, outcomev, num1 = "best"){
    
    #Reads Data
    
    outcome.data <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE)
    
    #Terminates program for invalid arguments
    
    if(!(statev %in% outcome.data$State)){
        
        stop("invalid state")
    }
    
    
    if(!(outcomev == "heart attack" | outcomev == "heart failure" | outcomev == "pneumonia")){
        
        stop("invalid outcome")
    }
    
    #Extracts required columns for analysis and renames the columns
    
    processed.data <- outcome.data[ , c(2, 7, 11, 17, 23)]
    
    colnames(processed.data) <- c("hospital", "state", "attack", "failure", "pneumonia")
    
    splitData <- split(processed.data, processed.data$state)
    
    split.state <- as.data.frame(splitData[statev])
    
    colnames(split.state) <- c("hospital", "state", "attack", "failure", "pneumonia")
    
    #Removes NA values
    
    good <- complete.cases(split.state)
    
    split.state <- split.state[good, ]
    
    
    #Assigns num1 character values to integers
    
    if(num1 == "best"){
        
        num1 <- 1
    }
    
    else if(num1 == "worst"){
        
        num1 <- nrow(split.state)
    }
    
    else{
        
        a <- "this is random"
    }
    
    #Selects the hospital based on num1 and breaks ties by alphabetical order using order()
    
    if(outcomev == "heart attack"){
        
        lowest.mortality <- split.state[order(split.state$attack, split.state$hospital), ]
        
        best.hospital <- lowest.mortality$hospital[num1]
    }
    
    else if(outcomev == "heart failure"){
        
        lowest.mortality <- split.state[order(split.state$failure, split.state$hospital), ]
        
        best.hospital <- lowest.mortality$hospital[num1]
        
    }
    
    else if(outcomev == "pneumonia"){
        
        lowest.mortality <- split.state[order(split.state$pneumonia, split.state$hospital), ]
        
        best.hospital <- lowest.mortality$hospital[num1]
        
    }
    
    
    #Returns the hospital called by num1, breaking ties by alphabetical order

    
    best.hospital
    
}