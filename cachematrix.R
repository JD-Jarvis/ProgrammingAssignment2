

## this function creates a cache that the matrix solution can be stored in


makeCacheMatrix <- function(ma = matrix()) {
  im <- NULL
  setMatrix <- function(y) {
    ma <<- y
    im <<- NULL
  }
  getMatrix <- function() ma
  setinverse <- function(inv) im <<- inv
  getinverse <- function() im
  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       setinverse = setinverse,
       getinverse = getinverse)
}

## this function takes the makecachematrix(x) as an input and outputs either 
## the stored answer or the new answer and stores it. 
cacheSolve <- function(x, ...) {
  im <- x$getinverse()
  if (!is.null(im)) {
    message("getting cached inverse matrix")
    return(im)
  }
  data <- x$getMatrix()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
## Return a matrix that is the inverse of 'x'

