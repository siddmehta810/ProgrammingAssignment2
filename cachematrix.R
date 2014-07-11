## this file does caching of the inverse of a matrix

##this function takes a matrix ( say, mat) as an input to create a special matrix object which is used for caching of its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##this function takes the object create in the above function to compute the inverse of the matrix, mat. 
##And if its inverse is already present in the cache then it directly orint from the cache, rather than computing it. 
##retuns the inverse of the matrix
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
