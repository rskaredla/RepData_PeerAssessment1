## Put comments here that give an overall description of what your
## functions do

## On calling this function, the function stores a matrix [get <- function() x]
## & it's inverse. The inverse is equal to its intialized value
## (NULL) or a value that a user assigns by the command - 
## var_mcm$setinv(varinv). The command 'invM <<- inverse' assigns
## the inverse value from Global environment to 'invM'
## The function returns a list. 

makeCacheMatrix <- function(x = matrix()) {
  invM <- NULL  
  set <- function(y) {
    x <<- y
    invM <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) invM <<- inverse
  getinv <- function() invM
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This functions returns the inverse of a matrix which has been 
## previously defined by makeCacheMatrix. If the inv of the matrix
## is not NULL (initial value defined in makeCacheMatrix), the 
## inverse is got from the cache, if NULL, the inverse is calculate
## by the solve command, also this is stored in the cache by the 
## x$setinv(invM).
cacheSolve <- function(x, ...) {
  invM <- x$getinv()
  if(!is.null(invM)) {
    message("getting cached data")
    return(invM)
  }
  data <- x$get()
  invM <- solve(data, ...)
  x$setinv(invM)
  invM
}
