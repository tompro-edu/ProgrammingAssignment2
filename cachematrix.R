## Based on the examples provided functions are modified to work on matrices

## makeCacheMatrix supports setting a matrix and a cache of inverse matrix
## there's also a get/set function for the internally stored matrix x

makeCacheMatrix <- function(x = matrix()) {

      invX <- NULL
      set <- function(y) {
            x <<- y
            invX <<- NULL
      }
      get <- function() x
      setInverse <- function(inverseM) invX <<- inverseM
      getInverse <- function() invX
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)      
      
}


## cacheSolve works on cacheMatrices
## it will attempt to get the cached inverse of matrix and return it if possible
## if not it will be calculated, same as mean value in the vector example

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getInverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      invX <- solve(data, ...)
      x$setInverse(invX)
      invX      
}
