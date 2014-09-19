## Provides caching functionality for matrix inversion.

## Given a matrix, returns a list of 4 functions: get/set/setinverse/getinverse.
## Provides internal cache for inverted matrixes.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Given a matrixCacheMatrix that MUST be invertible,returns its inverted matrix.
## It returns a cached inverted matrix if it was previously computed.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
