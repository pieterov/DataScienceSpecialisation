rm(list=ls())
setwd("/Users/Pieter/Library/Mobile Documents/com~apple~CloudDocs/2.R/DataScienceCoursera/3. Getting and Cleaning Data/Week 2")
dir()

# Question 1
# Answer is '2013-11-07T13:25:07Z'


# Question 2: https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv
#
# Install MySQL: http://dev.mysql.com/doc/refman/5.7/en/osx-installation-pkg.html
# 2016-11-25T21:03:14.074627Z 1 [Note] A temporary password is generated for root@localhost: pqTe1qdp&fk)
# If you lose this password, please consult the section How to Reset the Root Password in the MySQL reference manual.
#
# Is there a need for installation of MySQL above? First try install.packages w/o the above.
install.packages("RMySQL")
install.packages("sqldf")
library(RMySQL)
library(sqldf)

file <- "ss06pid.csv"
acs <- read.csv(file)

sqlstring <- "select pwgtp1, AGEP from acs"
df <- sqldf(sqlstring)

df1 <- sqldf("select pwgtp1 from acs where AGEP < 50")
df2 <- sqldf("select * from acs")
df3 <- sqldf("select pwgtp1 from acs")
df4 <- sqldf("select * from acs where AGEP < 50 and pwgtp1")
# Answer is df1


# Question 3: 
df <- unique(acs$AGEP)

df1 <- sqldf("select distinct AGEP from acs")
df2 <- sqldf("select AGEP where unique from acs")
df3 <- sqldf("select distinct pwgtp1 from acs")
df4 <- sqldf("select unique AGEP from acs")
# Answer is df1


# Question 4: http://biostat.jhsph.edu/~jleek/contact.html
htmlstring <- "http://biostat.jhsph.edu/~jleek/contact.html"
connection <- url(htmlstring)
htmlCode <- readLines(connection)
close(connection)
nchar(htmlCode)
# Answer is '45 31 7 25'


# Questions 5:
filename <- "getdata-wksst8110.for"
# filename <- url("http://www.cpc.ncep.noaa.gov/data/indices/wksst8110.for"
# http://stackoverflow.com/questions/14383710/read-fixed-width-text-file
# the -1 skips a character.
df1 <- read.fwf(file=filename, widths=c(-1,9,-5,4,-1,3,-5,4,-1,3,-5,4,-1,3,-5,4,-1,3), skip=4)
df2 <- read.fwf(file=filename, widths=c(-1,9,-5,4,4,-5,4,4,-5,4,4,-5,4,4), skip=4)

sum(df1[,c(4,9)])
tail(df1)
tail(df2)

# alternative, apparently quicker, but read.fwf seems fast enough
install.packages("readr")
library(readr)
x <- read_fwf(
        file="http://www.cpc.ncep.noaa.gov/data/indices/wksst8110.for",   
        skip=4,
        fwf_widths(c(12, 7, 4, 9, 4, 9, 4, 9, 4)))

# Answer is 32426.7

