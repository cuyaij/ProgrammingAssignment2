#Programming Assignment# 2: Lexical Scoping
#Repeatedly computing matrix inversions is costly and these functions assist by caching the inverse of a matrix.

#makeCacheMatrix: creates a special matrix that caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) m <<- inverse
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

#cacheSolve: computes the inverse of speical matrix computed by makeCacheMatrix and retrieves the inverse from the cache if it is already calculated.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- inverse(data, ...)
  x$setinv(m)
  m
}
