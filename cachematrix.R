## Put comments here that give an overall description of what your
## functions do

## This function creates a special "cache", caontaing function to
## 1. Set the value of cache
## 2. Get the value of cache
## 3. Set the value of the inverse
## 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  p <- NULL
  set <- function(y) {
    x <<- y
    p <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse)p <<- inverse
  getInverse <- function()p
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  p <- x$getInverse()
  if(!is.null(p)) {
      message("getting cached data")
      return(p)
  }
  data <-x$get()
  p <- solve(data, ...)
  x$setInverse(p)
  p
}
