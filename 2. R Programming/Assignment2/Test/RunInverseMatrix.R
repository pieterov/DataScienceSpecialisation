root = "/Users/Pieter/Library/Mobile Documents/com~apple~CloudDocs/2.R/DataScienceCoursera/"
path = "2. R Programming/ProgrammingAssignment2/Test"
setwd(paste(root,path,sep=""))

source('cachematrix.R')
source('FunctionDuration.R')

testmatrix <- matrix(rnorm(1000000),nrow=1000,ncol=1000)

cachmat <- makeCacheMatrix(testmatrix)

result <- FunctionDuration(cacheSolve, cachmat)
print(result[[1]])

result <- FunctionDuration(cacheSolve, cachmat)
print(result[[1]])

result <- FunctionDuration(solve, testmatrix)
print(result[[1]])