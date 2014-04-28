## Put comments here that give an overall description of what your
## functions do

## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## * set the value of the matrix
## * get the value of the matrix
## * set the value of the inverse
## * get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  cachesolve <- NULL
  set <- function(y) {
    x <<- y
    cachesolve <<- NULL
  }
  get <- function() x
  setSolve <- function(s) cachesolve <<- s
  getSolve  <- function() cachesolve
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


## The secound function, cacheSolve calculates the inverse of the special "matrix" created with the above function. 
## * if there is a `cachesolve` just return it
## * otherwise solve and store the result to `cachesolve` and return it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  cachesolve <- x$getSolve()
  if(!is.null(cachesolve)) {
    message("getting cached data")
    return(cachesolve)
  }
  data <- x$get()
  cachesolve <- solve(data, ...)
  x$setSolve(cachesolve)
  cachesolve
}
