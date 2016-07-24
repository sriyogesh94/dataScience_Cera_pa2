best <- function(statev, outcomev){
    
    outcome.data <- read.csv("outcome-of-care-measures.csv")
    
    if(!(statev %in% outcome.data$State)){
        
        stop("invalid state")
    }
    
    
    if(!(outcomev == "heart attack" | outcomev == "heart failure" | outcomev == "pneumonia")){
        
        stop("invalid outcome")
    }
    
    "success"

}