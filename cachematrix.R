## Put comments here that give an overall description of what your
## functions do:
##
## The functions listed in this file allow you to input a Matrix 
## and create a list that is able to cache the inverse of the matrix
## itself. If the inverse of the Matrix needs to be computed several times than
## the cached inverse value is returned rather than compute it repeatedly.

## Write a short comment describing this function:
##
## The function "makeCacheMatrix" is used to create a special object, a list, that stores a
## a matrix and cache's its inverse. This function has as argument an invertible square matrix.
## The list object returned by "makeCacheMatrix" containes the value of the original matrix and can 
## chace its inverse. 


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## Write a short comment describing this function:
##
## The function "cacheSolve" calculates the inverse of the special "matrix" (a list object) returned 
## by the function "makeCacheMatrix" above. If the inverse has already been calculated (and the matrix
## has not changed), then the "cacheSolve" retrieve the inverse from the cache and print the message 
## "getting cached data".


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
