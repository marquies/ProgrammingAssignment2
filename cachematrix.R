## Helper functions to use for caching inverse of a given matrix.
## Create objects with makeCacheMatrix and use cacheSolve to calc and cache
## the inverse.

## Creates a list as holder object for matrix and cached inverse.
## Use cacheSolve to calculate the matrix and cache the result in the given 
## matrix.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Calculates the inverse and caches in the object.
cacheSolve <- function(x, ...) {
  matrix <- x$getInverse()
  if (!is.null(matrix)) {
    message("getting cached data")
    return (matrix)
  }
  data <- x$get()
  matrix <- solve(data)
  x$setInverse(matrix)
  matrix
}
