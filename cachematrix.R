## Computing the inverse of a matrix is computationally intensive, making
## it worthwhile to cache calculated inverses for reuse, rather than 
## recalculating at each required place.

## This pair of functions are capable of caching and solving the inverse
## of a matrix.

## makeCacheMatrix caches the calculated inverse

makeCacheMatrix <- function(x = matrix()) {
        # Create a null matrix
        minv <- NULL
        # User can set the invertible matrix
        set <- function(y) {
                x <<- y
                # Reset in this case the inverse matrix to NULL
                minv <<- NULL
        }
        # Return the stored invertible matrix
        get <- function() x
        # Caching the inverse of the matrix
        setinverse <- function(inverse) minv <<- inverse
        # Returning the inverse (null or otherwise) of the matrix
        getinverse <- function() minv
        # List of functions within makeCacheMatrix
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve solves for the matrix inverse if unavailable

cacheSolve <- function(x, ...) {
        # Get inverse (null or cached) from makeCacheMatrix
        minv <- x$getinverse()
        # If not null inverse, then return the cached inverse
        if (!is.null(minv)) {
                message("Getting cached data") # Cache confirmation
                return(minv)
        }
        # Get the invertible matrix
        data <- x$get()
        # Solve for its inverse
        minv <- solve(data, ...)
        # Cache its inverse
        x$setinverse(minv)
        # Print the calculated inverse
        minv
}