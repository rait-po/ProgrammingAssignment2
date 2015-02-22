## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## I need to create vector/list of functions to be used by function "cacheSolve" 
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the solve
## 4.get the value of the solve

makeCacheMatrix <- function(x = matrix()) {
  cmem <- NULL
  set <- function(y) {
    x <<- y
    cmem <<- NULL
  }
  get <- function() x
  setsolved <- function(solve) cmem <<- solve
  getsolved <- function() cmem
  list(set = set, get = get,
       setsolved = setsolved,
       getsolved = getsolved)
}


## Write a short comment describing this function
## This will call the functions defined in "makeCacheMatrix" to apply caching functionality

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  cmem <- x$getsolved()
  if(!is.null(cmem)) {
    message("getting cached data")
    return(cmem)
  }
  data <- x$get()
  cmem <- solve(data, ...)
  x$setsolved(cmem)
  cmem
}
