pollutantmean <- function(directory, pollutant, IDvector=1:332) {

  setwd(directory)
  
  files <- list.files(pattern="*.csv")
  
  mergeddata <- read.csv(files[1])
  
  for (i in 2:length(files)) {
    mergeddata <- rbind(mergeddata,read.csv(files[i]))}
  
  datasubset <- mergeddata[is.element(mergeddata$ID,IDvector),]
  
  setwd('..')
  
  write.csv(mergeddata,file='mergeddata.csv')
  write.csv(datasubset,file='datasubset.csv')
  
  pollutantmean <- mean(datasubset[[pollutant]], na.rm = TRUE)
  
}