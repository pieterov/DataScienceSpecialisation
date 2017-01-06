##cachematrix.R
##NORDIN RAMLI, 6 November 2016
##This work is related to a set of functions that can cache the inverse of a matrix.

##              THE USAGE
##              ---------
##  Creates an object with the cache of a square matrix and creates
##  a list of functions for interacting with the matrix.
##
##  - set        Creates a cache of the matrix upon the call of
##               makeCacheMatrix and clears any previous cached
##               inverse matrix
##  - get        Returns the cached matrix
##  - setInverse     Creates a cache of the solved inverse of the
##               orignal cached matrix created by set
##  - getInverse     Returns the cached inverse matrix
##
##      EXAMPLE
##      ---------
##> mat<-matrix(c(1:4),c(2,2))
##> mat
##[,1] [,2]
##[1,]    1    3
##[2,]    2    4
##> cacheMat<-makeCacheMatrix(mat)
##> cacheMat$get()
##[,1] [,2]
##[1,]    1    3
##[2,]    2    4
##> cacheMat$getInverse()
##NULL
##> cacheSolve(cacheMat)
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##> cacheSolve(cacheMat)
##Get the cached inverse matrix
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##> cacheMat$getInverse()
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##>
##-------------------------------
makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set<-function(y){
                x<<-y
                inv<<-NULL
        }
        get<-function() x
        setInverse<-function(solveMatrix) inv<<-solveMatrix
        getInverse<-function() inv
        list(set=set,
             get=get,
             setInverse=setInverse,
             getInverse=getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getInverse()
        if(!is.null(inv)){
                message("Get the cached inverse matrix")
                return(inv)
        }
        data<-x$get()
        inv<-solve(data)
        x$setInverse(inv)
        inv
}