## Hello and Welcome
## Coursera Programming Assignment 2: Lexical Scoping

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  mtrx <- NULL
  set <- function(y) {
    x <<- y
    mtrx <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) mtrx <<- solve
  getinverse <- function() mtrx
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  mtrx <- x$getinverse()
  if(!is.null(mtrx)) {
    message("Coming right up")
    return(mtrx)
  }
  data <- x$get()
  mtrx <- solve(data, ...)
  x$setinverse(mtrx)
  mtrx
}

