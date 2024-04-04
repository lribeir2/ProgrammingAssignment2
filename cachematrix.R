## Create a special 'matrix' object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    ## Function to set the matrix
    set <- function(y) {
        x <<- y
        inverse <<- NULL  # Reset the cached inverse when the matrix is changed
    }
    
    ## Function to get the matrix
    get <- function() {
        x
    }
    
    ## Function to set the cached inverse
    setInverse <- function(inverseMatrix) {
        inverse <<- inverseMatrix
    }
    
    ## Function to get the cached inverse
    getInverse <- function() {
        inverse
    }
    
    ## Return a list with the defined functions
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Compute the inverse of the special 'matrix' object
cacheSolve <- function(x, ...) {
    ## Retrieve the cached inverse if available
    inverse <- x$getInverse()
    
    ## If the cached inverse exists, return it
    if (!is.null(inverse)) {
        message("Retrieving cached inverse")
        return(inverse)
    }
    
    ## If the cached inverse does not exist, compute it and cache it
    data <- x$get()
    inverse <- solve(data, ...)
    
    x$setInverse(inverse)  # Cache the inverse
    
    inverse  # Return the computed inverse
}
