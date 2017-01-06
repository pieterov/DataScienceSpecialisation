rm(list=ls())

# Question 1: https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv
data <- read.csv("homes.csv")
print(table(data$VAL))
# Answer is 53

# Question 2:
# Answer is "Each variable in a tidy data set has been transformed to be interpretable." Not right.
# Anser is "Tidy data has one variable per column." RIGHT

# Question 3: https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx
install.packages("XLConnect")
library(XLConnect)
# download JAVA through: https://support.apple.com/kb/DL1572?viewlocale=en_US&locale=en_US
wb <- loadWorkbook("Natural Gas.xlsx")
data <- readWorksheet(wb, sheet = "NGAP Sample Data", startRow = 18, 
                      startCol = 7, endRow = 23, endCol = 15)
# Answer is 36534720

# Question 4: https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml
install.packages('XML')
library(XML)
fileURL <- "http://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"
doc <- xmlTreeParse(fileURL, useInternalNodes = TRUE)
rootNode <- xmlRoot(doc)
zipcodes <- xpathSApply(rootNode,"//zipcode",xmlValue)
names <- xpathSApply(rootNode,"//name",xmlValue)
sum(zipcodes=="21231")
# Answer is 127

# Questions 5: https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv","ACS.csv")
DT <- read.csv("ACS.csv")

ptm <- proc.time()
mean(DT[DT$SEX==1,]$pwgtp15); mean(DT[DT$SEX==2,]$pwgtp15)
proc.time() - ptm

ptm <- proc.time()
mean(DT$pwgtp15,by=DT$SEX)
proc.time() - ptm
# Not the answer

ptm <- proc.time()
rowMeans(DT)[DT$SEX==1]; rowMeans(DT)[DT$SEX==2]
proc.time() - ptm

ptm <- proc.time()
sapply(split(DT$pwgtp15,DT$SEX),mean)
proc.time() - ptm
# Not the answer

ptm <- proc.time()
tapply(DT$pwgtp15,DT$SEX,mean)
proc.time() - ptm
# Not the answer

ptm <- proc.time()
DT[,mean(pwgtp15), by=SEX]
proc.time() - ptm

