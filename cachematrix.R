## These functions first creates a list of functions that can be applied to a
## matrix, including setting the value of a matrix, returning the matrix,
## setting the value of a matrix's inverse, and returning that inverse. The
## second function takes the output of the first function as an argument and
## returns the inverse of the matrix. It also caches (sets the value of) the
## inverse. When asked to calculate the inverse of a matrix it has already seen,
## it calls the value from the cache rather than calculating it again.

## This function creates a list of functions to be applied to a matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function returns and caches the inverse of a matrix, or calls an already
## inverted matrix from the cache

cacheSolve <- function(x, ...) {
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