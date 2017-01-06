rm(list=ls())
setwd("/Users/Pieter/Library/Mobile Documents/com~apple~CloudDocs/2.R/DataScienceCoursera/3. Getting and Cleaning Data")

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

# 1. mean(DT[DT$SEX==1,]$pwgtp15); mean(DT[DT$SEX==2,]$pwgtp15)
# 2. mean(DT$pwgtp15,by=DT$SEX)
# 3. rowMeans(DT)[DT$SEX==1]; rowMeans(DT)[DT$SEX==2]
# 4. sapply(split(DT$pwgtp15,DT$SEX),mean)
# 5. tapply(DT$pwgtp15,DT$SEX,mean)
# DT[,mean(pwgtp15),by=SEX]


install.packages("data.table")
library(data.table)
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv","ACS.csv")
DT <- fread("ACS.csv")

result <- function(x) {
        ptm <- proc.time()
                S1 <- mean(DT[DT$SEX==1,]$pwgtp15)
                S2 <- mean(DT[DT$SEX==2,]$pwgtp15)
        ptm1 <- proc.time() - ptm

        ptm <- proc.time()
                sapply(split(DT$pwgtp15,DT$SEX),mean)
        ptm4 <- proc.time() - ptm
        
        ptm <- proc.time()
                tapply(DT$pwgtp15,DT$SEX,mean)
        ptm5 <- proc.time() - ptm
        
        ptm <- proc.time()
                DT[,mean(pwgtp15), by=SEX]
        ptm6 <- proc.time() - ptm
        
        result <- c(ptm1[3], ptm4[3], ptm5[3], ptm6[3])
        
        }   

## ====================================

df <- c()
dfavg <- c()

for (i in 1:1000) {
        df <- rbind(df, result(i)) 
        dfavg <- rbind(dfavg, colMeans(df))
}


# system.time(pt <- lapply(1:100,result))


# Option 1. PROVIDES BOTH AVERAGES. 
mean(DT[DT$SEX==1,]$pwgtp15); mean(DT[DT$SEX==2,]$pwgtp15)
# Option 2. Not the answer. Only gives overall average, not by sex.
mean(DT$pwgtp15,by=DT$SEX)
# Option 3. Not the answer. Does not give the correct average by column.
rowMeans(DT)[DT$SEX==1]; rowMeans(DT)[DT$SEX==2]
# Option 4. PROVIDES BOTH AVERAGES. Not the answer.
sapply(split(DT$pwgtp15,DT$SEX),mean)
# Option 5. PROVIDES BOTH AVERAGES. Not the answer.
tapply(DT$pwgtp15,DT$SEX,mean)
# Option 6. PROVIDES BOTH AVERAGES. THE RIGHT ANSWER.
DT[,mean(pwgtp15), by=SEX]
