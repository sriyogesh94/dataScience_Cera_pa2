best <- function(statev, outcomev){
    
    #Reads Data
    
    outcome.data <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE)
    
    #Terminates program for ivalid arguments
    
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
    
    #Collects hospitals with lowest mortality in the specified outcome
    
    if(outcomev == "heart attack"){
        
        lowest.mortality <- min(split.state$attack, na.rm = TRUE)
        
        lowest.mortality <- which(split.state$attack == lowest.mortality)
        
        best.hospital <- split.state$hospital[lowest.mortality]
    }
    
    else if(outcomev == "heart failure"){
        
        lowest.mortality <- min(split.state$failure, na.rm = TRUE)
        
        lowest.mortality <- which(split.state$failure == lowest.mortality)
        
        best.hospital <- split.state$hospital[lowest.mortality]
        
    }
    
    else if(outcomev == "pneumonia"){
        
        lowest.mortality <- min(split.state$pneumonia, na.rm = TRUE)
        
        lowest.mortality <- which(split.state$pneumonia == lowest.mortality)
        
        best.hospital <- split.state$hospital[lowest.mortality]
        
    }
    
    
    
    #Returns the best hospital sorted in alphabetical order
    best.hospital <- sort(as.vector(best.hospital))[1]
    
    best.hospital
    
}