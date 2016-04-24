# The purpose of this function is to cache the inverse matrix of a given matrix.
# It returns a list of the following functions:
# set:        sets the value of the matrix, and resets the inverse matrix cache
# get:        returns the value of the matrix
# setInverse: stores (caches) the value of the inverse matrix
# getInverse: returns the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  
  # this variable stores the current matrix
  # it is initialized with null
  value <- NULL
  
  # set the matrix value 
  set <- function(newValue) {
    x <<- newValue ## store new value
    value <<- NULL ## reset cache
  }
  
  # get the matrix value
  get <- function() x 
  
  # cache inverse matrix 
  setInverse <- function(inverseMatrix) value <<- inverseMatrix
    
  getInverse <- function() value
  
  # return a list of functions.
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


# This method returns the already cached inverse matrix. 
# If the inverse matrix data is not cached, the inverse matrix is calculated,
# then stored it in the cache, and finally returned.
# The x parameter is the makeCacheMatrix object passed to be processed.
cacheSolve <- function(x, ...) {
        
  m <- x$getInverse()
  
  # return the existing inverse matrix if possible
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # extract the matrix data
  data <- x$get()
  
  # compute and cache the inverse matrix
  m <- solve(data)
  x$setInverse(m)
  m
}
