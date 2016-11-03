## This function creates a special "matrix" object that can cache its inverse
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse



makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    setMatrix <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    getMatrix <- function() x
    setInverse <- function(inv) inverse <<- inv
    getInverse <- function() inverse
    list(setMatrix = setMatrix, getMatrix = getMatrix,setInverse = setInverse,getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        if(!is.null(inverse)){
            message("Getting Cached data.")
            return(inverse)
        }
        data <- x$getMatrix()
        inverse <- solve(data,...)
        x$setInverse(inverse)
        inverse
}
