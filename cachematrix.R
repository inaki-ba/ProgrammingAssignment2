## Put comments here that give an overall description of what your
## functions do

## This function creates a special vector that stores a list
## that contains functions to set and get the value of the vector,
## and also, set and get the value of the mean.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  
}


## This second function calculates and stores de mean of the sepcial
##  vector created in the function above, only if it has not been 
## calculated before. If it has, it uses the cached entry in order
## to skip the computation and save processing time.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
