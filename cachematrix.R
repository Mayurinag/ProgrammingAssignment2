## Functions for cached matrix inversion. 

## Creates a matrix object with cached inverse. Use the function cacheSolve on 
## this object to get the inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(i) inv <<- i
    getinv <- function() inv
    
    list(set = set, get = get, 
         setinv = setinv,
         getinv = getinv)
}

## Calculates the inverse of a matrix created by the makeCacheMatrix function.
## The function is cached.
cacheSolve <- function(x, ...) {
    # If the inverse has been cached, 
    # return the cached data.
    inv <- x$getinv()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    } 
    # Otherwise compute the inverse
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    # Return the inverse
    inv
}
