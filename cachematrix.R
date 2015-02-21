## These two functions I am creating here to fulfil the class work in coursera.
## Idea is to reduce the computation by cacheing the values from previous run.
## Programmer need to make sure when to cache and when to not as it may increase the memory usage.


## This function will set and get the metrix and set and get the inverse of matrix.
## It takes one argument as matrix and returns list of functions to support cache operations.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(i) inverse <<- i
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting inverse from cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
}
