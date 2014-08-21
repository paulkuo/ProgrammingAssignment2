## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special "matrix",
## which is really a list containing the following functions:
##  1. set the value of the matrix
##  2. get the value of the matrix
##  3. set the inverse of the matrix 
##  4. get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    inverseX <- NULL
    set <- function(m) {
        x <<- m
        inverseX <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inverseX <<- inverse
    getInverse <- function() inverseX
    list(set = set, get = get,
        setInverse = setInverse, getInverse = getInverse)
}

## This function calculates the inverse of the special "matrix" created with makeCacheMatrix().
## It first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the data,
## and sets the inverse of the matrix in the cache via setInverse().
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverseX <- x$getInverse()
    if (!is.null(inverseX)) {
        message("getting cached data")
        return(inverseX)
    }
    data <- x$get()
    inverseX <- solve(data)
    x$setInverse(inverseX)
    inverseX
}
