## Matrix inversion is usually a costly computation and there may
## be some benefit to caching the inverse of a matrix rather than
## compute it repeatedly.

## This function creates a special "matrix" object that can
## cache its inverse.

makeCatcheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setverse,
       getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" created by
## makeCacheMatrix above. If the inverse has already been calculated ( and the
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
          return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}