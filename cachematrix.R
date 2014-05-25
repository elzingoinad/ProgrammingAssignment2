## Helper functions for faster computation of matrix inversion by using
## a caching mechanism. The first computation of a matrix inverse will
## be slow, but subsequent calls to calculate the inverse of the same
## matrix will be fast.
## Usage:
## - create a special matrix with caching abilities using the
##   makeCacheMatrix function
## - calculate the inverse using the cacheSolve function


## Creates a special matrix with caching abilities, based on a
## 'normal' matrix x.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Computes the inverse of a 'special' matrix x (created with
## makeCacheMatrix. Upon subsequent calls for the same matrix
## the inverse is retrieved from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
