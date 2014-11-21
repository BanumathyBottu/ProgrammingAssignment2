## A pair of functions that cache the inverse of a matrix

## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(z = matrix()) {
  inv <- NULL
  ## Method to set the matrix
  set <- function(y) {
    z <<- y
    inv <<- NULL
  }
  get <- function() z
  setinverse <- function(inverse) inv <<- inverse
  ## Method to get the inverse of the matrix
  getinverse <- function() inv
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}

cacheSolve <- function(z, ...) {
   
   inv <- z$getinverse()
  if(!is.null(inv)) {
    message("Getting cached data")
    return(inv)
  }
  data <- z$get()
  ## Calculate the inverse using matrix multiplication
  inv <- solve(data, ...)
  ## Set the inverse to the object
  z$setinverse(inv)
  ## Return the matrix
  inv
}
