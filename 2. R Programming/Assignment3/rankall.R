rankall <- function(outcome, num = "best") {
        
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
       
        ## Check that state and outcome are valid
        if (!any(outcome == c("Heart Attack", "Heart Failure", "Pneumonia"))) {
                stop("invalid outcome.")
        }
        
        ## For each state, find the hospital of the given rank
        outcome <- paste("Hospital.30.Day.Death..Mortality..Rates.from.", 
                         sub(" ", ".", outcome), sep="")
        
        subset1 <- data.frame(data$Hospital.Name, data$State, as.numeric(data[[outcome]]))
        names(subset1) <- c("Hospital", "State", "Outcome")
        
        subset2 <- subset1[complete.cases(subset1),]
        subset3 <- split(subset2, subset2$State)
        

        
        result <- c()
        
        for (i in 1:length(subset3)) {
                
                subset4 <- subset3[[i]]
                subset5 <- subset4[order(subset4$Outcome, subset4$Hospital),] 
        
                if (num == "best")      {result <- rbind(result,subset5[1,])}
                if (num == "worst")     {result <- rbind(result,tail(subset5, n=1))}
                if (is.numeric(num))    {result <- rbind(result,subset5[num,])}
                
        }
        
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        
        result[,1:2]

}