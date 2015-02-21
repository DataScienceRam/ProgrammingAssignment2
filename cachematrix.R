## These two functions I am creating here to fulfil the class work in coursera.
## Idea is to reduce the computation by cacheing the values from previous run.
## Programmer need to make sure when to cache and when to not as it may increase the memory usage.


## This function will set and get the metrix and set and get the inverse of matrix.
## It takes one argument as matrix and returns list of functions to support cache operations.
makeCacheMatrix <- function(x = matrix()) {
  
  inverse <- NULL               # First time we call this function inverse is NULL                        
  
  set <- function(y) {          # This internal function is used to set the metrix from outside.
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() x           # This function will return the matrix.
  
  # Following functions will set and get inverse values
  setInverse <- function(i) inverse <<- i
  getInverse <- function() inverse
  
  # Following will return the list of functions.
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The following function check to see if the cached value is present. If yes returns otherwise calculates caches.

cacheSolve <- function(x, ...) {
  
  # Get the inverse from the functions retuned and see it is already present.
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting inverse from cached data")
    return(inverse)
  }
  
  # Looks like inverse is not present. Get the metrix.
  data <- x$get()
  
  # Calculate the inverse.
  inverse <- solve(data, ...)
  
  # Cache it
  x$setInverse(inverse)
  
  # Return the inverse value back to user
  inverse
}
