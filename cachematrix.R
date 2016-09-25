## Put comments here that give an overall description of what your
## functions do

## This function get a matrix argument and cache it in environment
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmat <- function(mat) m <<- mat
  getmat <- function() m
  list(set = set, get = get,
       setmat = setmat,
       getmat = getmat)
}


## This function cache the matrix returned by function solve(). 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmat()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmat(m)
  m
}
