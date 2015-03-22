##This file contains two functions that can help dealing with the matrix and its
##inverse efficiently by caching the inverse of a matrix rather than compute it 
##repeatedly.
##THESE FUNCTIONS ARE PROVIDED WITHOUT ANY WARRANTY


## makeCacheMatrix takes one argument x beeng a matrix and crate a special object
##can cache a matrix and its inverse. The function returnes a list of functions to
#set and get the matrix; and to set and get its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## cacheSolve: returnes the inverse 
#of the special "matrix" object returned by makeCacheMatrix. 
#In the case when the inverse is already computed and the matrix has not changed,
#then the inverse from the cash is returned. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached the inverse matrix of x")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
