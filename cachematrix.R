## Acknowledgement: Browsed through stackoverflow and couple of websites to understand 
## usage of <<- operator through similar kind of functions.

## This function enables caching of its returned value, so that for any more request
## for the value, it will be fetched from cache, provided the passed matrix has not changed

makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  setMatrix <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  getMatrix <- function() x
  setInvMatrix <- function(inverse) invMatrix <<- inverse
  getInvMatrix <- function() invMatrix
  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       setInvMatrix = setInvMatrix,
       getInvMatrix = getInvMatrix)
}

## This function would find the inverse of passed matrix. If the inverse is available in the
## cache, the cached inverse will be returned. If so, there will be message as "From cache."
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invMatrix <- x$getInvMatrix()
  if (!is.null(invMatrix)) {
    message("From cache.")
    return(invMatrix)
  }
  mymatrix <- x$getMatrix()
  invMatrix <- solve(mymatrix, ...)
  x$setInvMatrix(invMatrix)
  invMatrix
}
