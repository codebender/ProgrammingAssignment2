## makeCacheMatrix creates a special "vector", which is really a list
## containing a function to:
##    set the value of the matrix
##    get the value of the matrix
##    set the inverse of the matrix
##    get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve will calculate the inverse of the matrix and cache the value
## if the same matrix inverse is requested again, it will return the cached
## value instead of recaluclating it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse
  if(!is.null(m)){
    print("returning cached inverse of matrix")
    return(m)
  }

  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  return(m)
}
