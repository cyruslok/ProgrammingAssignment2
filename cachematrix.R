## This function creates a special matrix cache its inverse
makeCacheMatrix <- function(x = matrix()) {  ## define the default mode of matrix
  inv <- NULL                                ## initialize inv = NULL, hold matrix inverse
  set <- function(y) {                       ## define set function
    x <<- y                                  ## value of matrix in parent environment
    inv <<- NULL                             ## if is a new matrix, reset inv
  }
  get <- function() x                        ## define  get fucntion returns matrix 
  
  setinverse <- function(inverse) inv <<- inverse ## assigns inv in parent environment
  getinverse <- function() inv                    ## gets  value of inv  
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) ## order to refer 
}

## computes the inverse of  matrix returned by makeCacheMatrix()
## If inverse has  been calculated and the matrix has not changed,
## then cacheSolve() will retrieve from the cache
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of x
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("get -> cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}