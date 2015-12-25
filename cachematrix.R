##Matrix inversion is usually a costly computation and there may be some benefit
##to caching the inverse of a matrix rather than compute it repeatedly. This
##pair of functions that caches the inverse of a matrix.

##makeVector creates a special object wrapper for a matrix, which is really a
##list containing functions to operate on the matrix:
## set: the matrix
## get: the matrix
## setinverse: to set the cached inverse of the matrix
##   Note: setinverse should not be called directly as it has no validation. It
##   is intended for use by cacheSolve
## getinverse: to get the cached inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##cacheSolve returns the inverse of the wrapped matrix. It will set the stored
##inverse if it was not previously set.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
