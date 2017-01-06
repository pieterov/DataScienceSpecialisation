corr <- function(directory, threshold = 0) {
        
        setwd(directory)
  
        files <- list.files(pattern="*.csv")

        corr <- c()

        for (i in 1:length(files)) {
                data <- read.csv(files[i])
                datasub <- data[!is.na(data$sulfate) & !is.na(data$nitrate),]
                
                if (nrow(datasub) > threshold) {
                        
                        corr <- append(corr, cor(datasub$nitrate, datasub$sulfate))   
                        
                }

        }

        setwd('..')

        corr
        
}