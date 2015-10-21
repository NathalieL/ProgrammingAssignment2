## The following functions cache the inverse of a matrix using the makeCacheMatrix function
## and cacheSolve function.

## The makeCacheMatrix function creates a matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The cacheSolve function computes the inverse of the matrix returned by makeCacheMatrix.

cacheSolve <- function(x, ...) {
        
  # if the inverse has already been calculated, 
  # retrieve the inverse from the cache
  
  i <- x$getinverse() 
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  } 
  
  #if the inverse has not already been calculated,
  #compute the inverse and return the result
  
  data <- x$get()
  i <- solve(data,...)
  x$setinverse(i)
  i
}
