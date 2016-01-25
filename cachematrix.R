## These functions cache the inverse of a matrix.

## makeCacheMatrix creates a special "matrix" 
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(invrs) inv <<- invrs
  getinv <- function() inv
  ## return cache of the matrix
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve computes the inverse of a 
## a "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated, 
## then the cachesolve should retrieve the
## inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Check to see if the matrix already has an inverse
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ## Return a matrix that is the inverse of 'x'
  data <- x$get()
  invrs <- solve(data)
  x$setinv(invrs)
  invrs
}


## Test Case
## = matrix(1:4,2,2)
## xcache = makeCacheMatrix(x)
## str(xcache)
## cacheSolve(xcache)