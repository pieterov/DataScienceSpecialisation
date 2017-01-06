root = "/Users/Pieter/Library/Mobile Documents/com~apple~CloudDocs/2.R/DataScienceCoursera/"
path = "2. R Programming/ProgrammingAssignment2/Test"
setwd(paste(root,path,sep=""))

source('cachemean.R')
source('makeVector.R')

ptm <- proc.time()
v <- rnorm(10000000)
w <- cachemean(makeVector(v))
print(w)
print(proc.time() - ptm)

ptm <- proc.time()
w <- cachemean(makeVector(v))
print(w)
print(proc.time() - ptm)
