## This function will use internal methods to create a cache version of the inverse of a matrix,
## by using lexical scoping

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(invMatrix) m <<- invMatrix
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function looks for a cache of the inverse matrix, if the cache is there, it returns it
## if it is not cached, it calculates the inverse of the matrix and caches it

cacheSolve <- function(x, ...) {
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
