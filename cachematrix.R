## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The first function, makeCacheMatrix creates a "vector, which is really a list containing a function to
##  set the matrix
##  get the matrix
##  set the inverse matrix
##  get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  Inv <- NULL
  set <- function(y) {
    x <<- y
    Inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) Inv <<- inverse
  getinverse <- function() Inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

## The following function calculates the inverse of the matrix 
## created with the above function. 
## However, it first checks to see if the inverse matrix has already been calculated. 
## If so, it gets the inverse matrix from the cache and skips the computation. 
## Otherwise, it solves the calculates the inverse matrix and sets the inverse matrix
## in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  Inv <- x$getinverse()
  if(!is.null(Inv)) {
    message("getting cached data")
    return(Inv)
  }
  data <- x$get()
  Inv <- solve(data, ...)
  x$setinverse(Inv)
  Inv
}
