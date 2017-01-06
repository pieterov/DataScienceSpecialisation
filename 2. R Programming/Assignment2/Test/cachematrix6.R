##a pair of functions that cache the inverse of a matrix.

##makeCacheMatrix: This function creates a special "matrix" 
##object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        ##initializing inverse
        inv <- NULL
        
        ##method to create a matrix (constructor)
        set <- function(mtr)
        {
                x <<- mtr     ##storing value of meatrix
                inv <<- NULL
        }
        
        ##method to get matrix
        get <- function()
        {
                ##returning the matrix
                x
        }
        
        
        ##method to set inverse of the matrix
        setInverse <- function(inp)
        {
                ##taking inp as input and storing the same to inv
                inv <<- inp
        }
        
        
        ## Method to get inverse of the matrix
        getInverse <- function()
        {
                ##returning inverse
                inv
        }
        
        ##returning set of methods implemented 
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
        
}


##acheSolve: This function computes the inverse of the special 
##"matrix" returned by makeCacheMatrix above. If the inverse has 
##already been calculated (and the matrix has not changed), then 
##the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        ##getting a matrix that is inverse of x (m)
        m <- x$getInverse()
        
        ##if m is not null, then we can say that the inverse 
        ## is already computed and cached
        if(!is.null(m))
        {
                message("getting cached data")
                return(m)
        }
        
        ## Get the matrix from our object
        data <- x$get()
        
        ## Calculate the inverse using matrix multiplication if matrix is not cached
        m <- solve(data) %*% data
        
        ## Setting the inverse to the object after it's computation
        x$setInverse(m)
        
        ## Return a matrix that is the inverse of 'x'
        m
}