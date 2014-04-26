## This couple of functions create a special type of matrix
## able to store its own inverse, and propose a modified version
## of the method solve() to take advantage of that cache.

## Creates a special matrix incorporating a cached
## version of its inverse. The matrix posesses four
## methods: get, set, getInverse and setInverse.
## At initialization, the inverse is set to NULL.
##
## Parameters: x: matrix.
## Returns: special matrix capable of caching its inverse.
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL # Inverse of the matrix x
    set <- function(y) {
        # Sets the matrix according to the parameter
        # and sets its inverse to NULL.
        x <<- y
        i <<- NULL
    }
    get <- function() x # Returns the matrix itself
    setInverse <- function(inverse) i <<- inverse # Sets the inverse according to the parameter
    getInverse <- function() i # Returns the inverse of the matrix
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Modified version of the method Solve(matrix()) using
## the special matrix format created by the method
## makeCacheMatrix(). Instead of systematically computing
## the inverse of the given matrix, it first checks if the
## said matrix has it already cached. If so, no need to
## compute it, simply return the cached version.
## If not, compute the inverse and cache it.
##
## Parameter: x: matrix created using makeCacheMatrix()
## Returns: Inverted matrix of x
cacheSolve <- function(x, ...) {
    i <- x$getInverse() # Get the cached inverse from x.
    if(!is.null(i)) { # Check if it's NULL.
        # It's not, return the cached inverse.
        message("Inverting matrix using the cached version...")
        return(i)
    }
    # If we get here, that means that the invert
    # has not been computed yet. Let's do this.
    message("Inverting matrix and caching it...")
    i <- solve(x$get()) # Inversion
    x$setInverse(i) # Saving in cache.
    return(i) # Return inverse.
}
