rankhospital <- function(state, outcome, num = "best") {
        
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        dataState <- data[data$State==state,]
        
        ## Check that state and outcome are valid
        if (!any(state==state.abb)) {
                stop("invalid state.")
        }
        if (!any(outcome == c("Heart Attack", "Heart Failure", "Pneumonia"))) {
                stop("invalid outcome.")
        }
        
        ## Return hospital name in that state with the given rank 30-day death rate
        outcome <- paste("Hospital.30.Day.Death..Mortality..Rates.from.", 
                         sub(" ", ".", outcome), sep="")
        
        subset1 <- data.frame(dataState$Hospital.Name, as.numeric(dataState[[outcome]]))
        subset2 <- subset1[complete.cases(subset1),]
        subset3 <- subset2[order(subset2[2], subset2[1]),] 
        
        if              (num == "best") {subset3[1,1]}
        else if         (num == "worst") {tail(subset3[,1], n=1)}
        else if         (is.numeric(num)) {subset3[num,1]}
        
}