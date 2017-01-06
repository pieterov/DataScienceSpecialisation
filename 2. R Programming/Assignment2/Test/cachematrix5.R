# This program creates and returns the inverse of a nonsingular matrix. 

# makeCacheMatrix is a function that creates a bunch functions and returns those 
# functions in a list in the parent environment. It starts by first initialzing
# matrix x and it's inverse, then setting them by removing any previous values
# attached to them, then sets the inverse, and finally gets the inverse. 
makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set<-function(y=matrix()){
                x<<-y
                inv<<-NULL
        }
        get<-function() x
        setinv<-function(solve) inv<<-solve
        getinv<-function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
        
}


#cacheSolve basically calls the getters and setters of the previous function
# using lexical scoping. Since the previous function returns a list, cacheSolve
# uses the list operator "$" to access the elements of the list. It first checks
# whether the inverse function is empty, if it is then it jumps down to last 4 
# lines and creates the inverse. 
cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
        
}