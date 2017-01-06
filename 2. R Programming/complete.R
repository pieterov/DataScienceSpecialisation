complete <- function(directory, id = 1:332) {
  
  setwd(directory)
  
  files <- list.files(pattern="*.csv")
  nobs <- numeric(length(id))
  
  for (i in 1:length(id)) {

    data <- read.csv(files[id[i]])
    
    nobs[i] <- sum(!is.na(data$sulfate) & !is.na(data$nitrate))
    }
  
  setwd('..')

  complete <- data.frame(id, nobs)
  
}