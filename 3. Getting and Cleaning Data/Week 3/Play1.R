rm(list=ls())
install.packages("dplyr")
library(dplyr)

setwd("/Users/Pieter/Library/Mobile Documents/com~apple~CloudDocs/2.R/DataScienceCoursera/3. Getting and Cleaning Data/Week 3")
dir()


chicago <- readRDS("chicago.rds")
str(chicago)
names(chicago)
chic.f <- filter(chicago, pm25tmean2 > 30)
head(chic.f)

chicago <- arrange(chicago, date)
head(chicago)
tail(chicago)
chicago <- arrange(chicago, desc(date))
head(chicago)
tail(chicago)

chicago <- rename(chicago, pm25 = pm25tmean2, dewpoint = dptp)
head(chicago)

chicago <- mutate(chicago, pm25detrend = pm25-mean(pm25, na.rm=TRUE))
head(select(chicago, pm25, pm25detrend))
head(chicago)

chicago <- mutate(chicago, tempcat = factor(1*(tmpd>80), labels = c("cold","hot")))
hotcold <-group_by(chicago, tempcat)
summarize(hotcold)

chicago <- mutate(chicago, year = as.POSIXlt(date)$year + 1900)
years <- group_by(chicago, year)
summarize(years, pm25 = mean(pm25, na.rm=TRUE), o3 = max(o3tmean2), no2=median(no2tmean2))

chicago %>% 
mutate(month = as.POSIXlt(date)$mon+1) %>% 
group_by(month) %>% 
summarize(pm25 = mean(pm25, na.rm = TRUE), o3 = max(o3tmean2), no2=median(no2tmean2))


