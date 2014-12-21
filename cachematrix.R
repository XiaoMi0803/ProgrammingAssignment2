## makeCacheMatrix() function creates  a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(Inverse) m <<- Inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
## cacheSolve() function calculates the inverse of the matrix created with 
## the above function. It first checks to see if the inverse of the matrix 
## has already been calculated. If so, it gets the inverse from the cache 
## and skips the computation. Otherwise, it calls the solve() function
## to calculates the inverse of the matrix and sets the value in the 
## cache via the setInverse() function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}