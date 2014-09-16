## This script contains a set of functions that handles a matrix
## and its inverse caching them to avoid high time-consuming
## computation.

## This function "prepares" the cache-matrix object to be
## handled in a cached way from an input matrix. Enables four
## functions (2 setters and two getters) to handle the cached
## matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      get <- function() x
      setInverse <- function(inverse) i <<- inverse
      getInverse <- function() i
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}


## Given a cache-matrix object returns the inverse of the matrix,
## getting it from the cache or generating and storing in cache
## if it is not in cache

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      m <- x$getInverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setInverse(m)
      m
}
