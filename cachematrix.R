## This file contains an implementation of a matrix object that caches it's inverse. 
## 
## Note: Following code is a modification of the example code provided
## in the assignment description.


## makeCacheMatrix is a constructor for a matrix that caches its inverse.
##
## Args:
##   x: Matrix to build the cache matrix object. Function assumes that x is invertible.
##
## Returns: 
##   A list that represents the cache matrix object. 


makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    ## the CacheMatrix represented as a list of getter/setter pairs 
    ## for the original matrix and the inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve returns the inverse of a cache matrix object. If the inverse hasn't been calculated, the
## result is cached. All subsequent calls return the cached result.
##
## Args:
##   x: A cache matrix object
##
## Returns: 
##   The inverse of the cache matrix object

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    ## This assumes that x is invertible
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
