## A pair of functions that cache the inverse of a matrix

## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  ## Initialize the inverse property
  ix <- NULL
  
  ## Method to set value to the matrix
  set <- function(y) {
    x <<- y
    ix <<- NULL
  }
  
  ## Method the get the matrix
  get <- function() x
  
  ## Method to set the inverse of the matrix
  setInverse <- function(inverse) ix <<- inverse
  
  ## Method to get the inverse of the matrix
  getInverse <- function() ix
  
  ## Return a list of the methods
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Get the cached value of the inverse matrix
  ix <- x$getInverse()
  
  ## If the cached value exists then return it
  if(!is.null(ix)) {
    message("getting cached data")
    return(ix)
  }
  
  ## Otherwise get the matrix from our object
  data <- x$get()
  
  ## Calculate the inverse
  ix <- solve(data, ...)
  
  ## Store the inverse matrix to cache
  x$setInverse(ix)
  
  ## Return the inverse matrix
  ix
}
