rm(list=ls())
install.packages("dplyr")
library(dplyr)

setwd("/Users/Pieter/Library/Mobile Documents/com~apple~CloudDocs/2.R/DataScienceCoursera/3. Getting and Cleaning Data/Week 3")
dir()

# QUESTION 1
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv","ACS survey.csv")
df <- read.csv("ACS survey.csv")
df <- mutate(df, agricultureLogical = (ACR == 3 & AGS == 6))
which(df$agricultureLogical)
# Answer: 125, 238, 262

# QUESTION 2
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg","picture.jpeg")
install.packages("jpeg")
library(jpeg)
picture <- readJPEG("picture.jpeg", native = TRUE)
quantile(picture, c(0.30, 0.80))
# Answer: -15259150 -10575416

# QUESTION 3
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv","GDP.csv")
gdp <- read.csv("GDP.csv", stringsAsFactors = FALSE)
gdp1 <- data.frame(gdp[,1], as.numeric(gdp[,2]), gdp[,4])
colnames(gdp1) <- c("ABB", "Rank", "Country")
gdp2 <- arrange(gdp1[complete.cases(gdp1),], desc(Rank))
answer2 <- gdp2[13,3]

download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv","EDU.csv")
edu <- read.csv("EDU.csv", stringsAsFactors = FALSE)
edu1 <- edu[,1]
answer1 <- length(intersect(gdp2[,1],edu1))

# Answer: 189, St Kitts

# QUESTION 4
edu2 <- edu[edu[,3]=="High income: nonOECD",c(1,3)]
colnames(edu2) <- c("ABB", "IncomeGroup")

edu3 <- edu[edu[,3]=="High income: OECD",c(1,3)]
colnames(edu3) <- c("ABB", "IncomeGroup")

edu_gdp2 <- mean((merge(gdp2, edu2, by='ABB'))$Rank)
edu_gdp3 <- mean((merge(gdp2, edu3, by='ABB'))$Rank)

# Answer: 32.96667, 91.91304

# QUESTION 5
gdp <- read.csv("GDP.csv", stringsAsFactors = FALSE)
gdp1 <- data.frame(gdp[,1], as.numeric(gdp[,2]), gdp[,4])
colnames(gdp1) <- c("ABB", "Rank", "Country")
gdp2 <- gdp1[complete.cases(gdp1),]

edu <- read.csv("EDU.csv", stringsAsFactors = FALSE)
edu2 <- edu[,c(1,3)]
colnames(edu2) <- c("ABB", "IncomeGroup")

edugdp0 <- (merge(gdp2, edu2, by='ABB'))
edugdp1 <- mutate(edugdp0, Cluster = ceiling(Rank/38))
edugdp2 <- arrange(edugdp1, Cluster, Rank)
table(edugdp2$Cluster, edugdp2$IncomeGroup)
edugdp2[edugdp2$Cluster == 1 & edugdp2$IncomeGroup == "Lower middle income",]

# Answer: 5