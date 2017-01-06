## This pair of functions will cache the inverse of a matrix.
## It will only compute the inverse of the matrix if the result i not cached before 
## The code i executed like this: MyMatrix <- makeCacheMatrix(c), where c is i matrix.
## After that you can run and rerun cacheSolve(myMatrix) as many times as you like, but it will only use 
## computing-power the first time you run it.


## This first function creates a special R object containing the matrix and some other variables in its own environment.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}