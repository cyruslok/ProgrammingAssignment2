## The purpose of these functions is to cache in memory
## the calculation for an inverse function since
## inverse functions can be computationally expensive

## This function creates an object in memory that stores
## the inverse matrix that is calculated using the
## solve function

## m holds the value of the inverse matrix
## x holds the original matrix

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
}


## This function uses lexical scoping to retrieve the
## inverse matrix by passing in the object created by
## makeCacheMatrix, the function() is called in the 
## makeCacheMatrix which returns m if it has been initialized
## if m has not been initialized, the function proceeds to call
## the solve function in makeCacheMatrix which calls
## the primative function solve for producing the inverse value.
## this value is stored in m within the global environment.  When
## cacheSolve is called a second time, it detects that m in the 
## global environment has been set and retrieves m from the global environment.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
  
}
