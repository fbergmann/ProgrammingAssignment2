# This simple file implements the makeCacheMatrix and the cacheSolve function
# as needed for programming assignment 2 of the r programming course. This code 
# pretty much follows the example from 'caching the mean of a vector' as given 
# in the description of the task. 
#
# To run you could try the following: 
#
#  mat  <- mat <- matrix(runif(400^2), 400); 
#  m    <- makeCacheMatrix(mat)
#  res0 <- cacheSolve(m)
#  res1 <- cacheSolve(m)
#
# Now you should have two identical results (res0 and res1), however
# the second time it should run it should have taken less time, and 
# you would see the printed message "getting cached data"
#
# author: Frank T. Bergmann
# date  : 2015/03/18
#


## makeCacheMatrix creates a special kind of 'matrix' that not ontly wraps
## the actual matrix, but also is able to cache the inverse of itself. 
makeCacheMatrix <- function(x = matrix()) {

  # initialize cache 
  i <- NULL
  
  # set matrix, initializing cache 
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  # return matrix
  get <- function() x
  
  # set the inverse
  setinverse <- function(inverse) i <<- inverse
  # return the cached inverse
  getinverse <- function() i
  
  # contain a list with all those functions
  list( set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


## The following function calculates the inverse of the special 'matrix' created 
## above. First it checks whether the inverse has already meen calculated. If so
## its value is returned, otherwise the inverse is calculated using the solve 
## function, and then its result is cached using the setinverse function. 

cacheSolve <- function(x, ...) {
  # get the inverse
  i <- x$getinverse()
  # if we have a cached value, return it
  if (!is.null(i)) {
    message ("getting cached data")
    return (i)
  }
  
  # otherwise get the actual matrix
  data <- x$get()
  # calculate its inverse
  i <- solve(data, ...)
  # cache the value
  x$setinverse(i)
  
  # and return the inverse as result
  i
}
