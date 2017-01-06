## Paired functions to create a special matrix which caches it's inverse and then retrieve it

## Function creates a special matrix which includes functions for caching and retrieving it's inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y)
        {
                x <<- y
                i <<- NULL
        }
        
        get <- function() x
        setInverse <- function(inverseMatrix) i <<- inverseMatrix
        getInverse <- function() i
        list(set = set, get = get, getInverse = getInverse, setInverse = setInverse)
        
}


## Function checks if the inverse exists and retrieves it if it does. Otherwise, it sets the inverse and then returns it 
## Need to update to handle the case of the matrix changing. Probably just flush the cached version so the function recalculates

cacheSolve <- function(x, ...) {
        
        i <- x$getInverse()
        if(!is.null(i))
        {
                print("cache hit")
                return (i)
        }
        else
        {
                print("cache miss")
                matrix <- x$get()
                inv <- solve(matrix)
                x$setInverse(inv) 
                x
        }
}