rm(list=ls())

setwd("/Users/Pieter/Library/Mobile Documents/com~apple~CloudDocs/2.R/DataScienceCoursera/3. Getting and Cleaning Data/Week 4")
dir()

# QUESTION 1
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv", "Idaho.csv")
df <- read.csv("Idaho.csv")
strsplit(names(df),"wgtp")[123]

# Answer: "" "15"


# QUESTION 2
# install.packages("data.table")
# library(data.table)
# df <- fread("GDP.csv") #, skip = 3, sep = ",")
# gives error: "Error: isLOGICAL(showProgress) is not TRUE"

download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv","GDP.csv")

all_content <- readLines("GDP.csv")
skip_content <- all_content[-1:-4]
df <- read.csv(textConnection(skip_content), header = TRUE, stringsAsFactors = FALSE)

df$GDP <- as.numeric(gsub(",","",df$X.4))
df$Rank <- as.numeric(df$X.1)

mean(df[df$Rank>0,"GDP"], na.rm=TRUE)
# Answer: 377652.4


# QUESTION 3
grep("^United",df$X.3)
# Answer: grep("^United",countryNames), 3

# QUESTION 4
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv","GDP2.csv")
df_GDP_all <- read.csv("GDP2.csv")
all_content <- readLines("GDP2.csv")
selection_content <- all_content[5:195]
df_GDP <- read.csv(textConnection(selection_content), header = TRUE, stringsAsFactors = FALSE)

download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv","EDU.csv")
df_EDU <- read.csv("EDU.csv")
sum(grepl("Fiscal year end: June",df_EDU$Special.Notes))
# Answer: 13


# QUESTION 5
install.packages('quantmod')
library(quantmod)

amzn = getSymbols("AMZN",auto.assign=FALSE)
sampleTimes = index(amzn)

sum(year(sampleTimes) == 2012)
sum(year(sampleTimes) == 2012 & weekdays(sampleTimes) == "Monday")
# Answer: 250, 47