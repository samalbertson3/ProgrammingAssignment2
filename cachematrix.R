## This file contains functions for generating, caching, and retrieving inverses of arbitrary, invertible matrices.

##creates a special data type for creating and caching matrix inverses

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  setMat <- function(y) {x <<-y}
  getMat <- function() {x}
  setInv <- function(y) {i <<- y}
  getInv <- function() {i}
  list(setMat=setMat, getMat=getMat, setInv=setInv, getInv=getInv)
}


##retrieves cached matrix inverses from makeCacheMatrix datatype

cacheSolve <- function(x, ...) {
  i <- x$getInv()
  if(is.null(i)){
    i <- solve(x$getMat())
    x$setInv(i)
    return(i)
  }
  else {
    #retrieve inverse
    print("getting cached data")
    return(x$getInv())
  }
}
