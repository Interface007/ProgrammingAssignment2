## This set of functions do provide a way to cache the work of calculating
## the inverse of a matrix (the method "solve" of R). 
## You might use this to get the matrix inverse multiple times from 
## a matrix while calculating it only once and reusingthe calculation
## result automatically without the need of an additional local variable.

## The makeCacheMatrix function does provide functionality to build the 
## data structure needed in order to use the function cacheSolve. It 
## accepts a matrix "x" and returns a list of functions to calculate 
## the matrix inversion and cache the result as long as the matrix has 
## not been changed through the "set" function of the list.
makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    
    get <- function() x
    
    setSolve <- function(solve) s <<- solve
    getSolve <- function() s
    
    list(set = set, 
         get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}


## This replacement of the native "solve" function does try to lookup a cached
## result from the argument "x". If it does not find a cached calculation 
## result it will do the calculation and store the result, so that it can 
## reuse it when called again with the same argument.
cacheSolve <- function(x, ...) {
    s <- x$getSolve()
    
    if(!is.null(s)) {
      message("getting cached data")
      return(s)
    }
    
    data <- x$get()
    s <- solve(data, ...)
    x$setSolve(s)
    s
}
