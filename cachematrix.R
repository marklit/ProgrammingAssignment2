# The following two functions will solve and cache the inverse of a Matrix.

makeCacheMatrix <- function(x = matrix()) {
    # Initialise the inverse Matrix
    m <- NULL

    # Return the Matrix
    getMatrix <- function() x

    # Set the Matrix
    setMatrix <- function(y) {
        x <<- y
        m <<- NULL
    }

    # Return the inverse of the Matrix
    getInverse <- function() m

    # Set the inverse of the Matrix
    setInverse <- function(inverse) m <<- inverse

    # Return a list of the methods within this function
    list(setMatrix  = setMatrix,
         getMatrix  = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
    # Return a Matrix that is the inverse of x

    # Attempt to return a cached version of the Matrix if it's available
    inverse_cached <- x$getInverse()

    if(!is.null(inverse_cached)) {
        message("Returning cached result")
        return(inverse_cached)
    }

    # If not cached version is available then compute the inverse of the
    # Matrix, cache it and return it.
    matrix_ <- x$getMatrix()
    inverse_ <- solve(matrix_, ...)
    x$setInverse(inverse_)
    inverse_
}
